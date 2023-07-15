package org.chiu.megalith.blog.controller;

import org.chiu.megalith.blog.vo.BlogDescriptionVo;
import org.chiu.megalith.blog.vo.VisitStatisticsVo;
import org.chiu.megalith.infra.bloom.handler.impl.*;
import org.chiu.megalith.infra.exception.NotFoundException;
import org.chiu.megalith.infra.bloom.Bloom;
import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.blog.vo.BlogExhibitVo;
import org.chiu.megalith.blog.vo.BlogHotReadVo;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Objects;

/**
 * @author mingchiuli
 * @create 2022-11-26 5:30 pm
 */
@RestController
@RequestMapping(value = "/public/blog")
@RequiredArgsConstructor
@Validated
public class BlogController {

    private final BlogService blogService;

    @Value("${blog.highest-role}")
    private String highestRole;

    @GetMapping("/info/{id}")
    @Bloom(handler = DetailHandler.class)
    public Result<BlogExhibitVo> getBlogDetail(@PathVariable(name = "id") Long id) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        BlogExhibitVo blog;

        if (Objects.nonNull(authentication)) {
            String authority = authentication.getAuthorities().stream()
                    .findFirst()
                    .map(GrantedAuthority::getAuthority)
                    .orElseThrow();

            if (("ROLE_" + highestRole).equals(authority)) {
                blog = blogService.findById(id, true);
            } else {
                blog = blogService.findById(id, false);
            }
        } else {
            blog = blogService.findById(id, false);
        }

        blogService.setReadCount(id);
        return Result.success(blog);
    }

    @GetMapping("/page/{currentPage}")
    @Bloom(handler = ListPageHandler.class)
    public Result<PageAdapter<BlogDescriptionVo>> getPage(@PathVariable(name = "currentPage") Integer currentPage,
                                                          @RequestParam(required = false, defaultValue = "-2147483648") Integer year) {
        PageAdapter<BlogDescriptionVo> page = blogService.findPage(currentPage, year);
        return Result.success(page);
    }

    @GetMapping("/locked/{blogId}")
    public Result<BlogExhibitVo> getLockedBlog(@PathVariable Long blogId,
                                               HttpServletRequest request) {

        String token = request.getHeader("token");
        boolean valid = blogService.checkToken(blogId, token);
        if (valid) {
            BlogExhibitVo vo = blogService.findById(blogId, true);
            blogService.setReadCount(blogId);
            return Result.success(vo);
        }
        return Result.fail("authorization exception");
    }

    @GetMapping("/status/{blogId}")
    @Bloom(handler = DetailHandler.class)
    public Result<Integer> getBlogStatus(@PathVariable Long blogId) {
        Integer status = blogService.findStatusById(blogId);
        return Result.success(status);
    }

    @GetMapping("/years")
    public Result<List<Integer>> searchYears() {
        List<Integer> years = blogService.searchYears();
        return Result.success(years);
    }

    @GetMapping("/stat")
    public Result<VisitStatisticsVo> getVisitStatistics() {
        VisitStatisticsVo vo = blogService.getVisitStatistics();
        return Result.success(vo);
    }

    @GetMapping("/scores")
    public Result<List<BlogHotReadVo>> getScoreBlogs() {
        List<BlogHotReadVo> hotList = blogService.getScoreBlogs();
        hotList.forEach(item -> {
            String title;
            Long id = item.getId();
            try {
                title = blogService.findById(id, false).getTitle();
            } catch (NotFoundException e) {
                title = blogService.findById(id, true).getTitle();
            }
            item.setTitle(title);
        });
        return Result.success(hotList);
    }

}
