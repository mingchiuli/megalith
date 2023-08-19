package org.chiu.megalith.blog.controller;

import org.chiu.megalith.blog.vo.BlogDescriptionVo;
import org.chiu.megalith.blog.vo.VisitStatisticsVo;
import org.chiu.megalith.infra.bloom.handler.impl.*;
import org.chiu.megalith.infra.exception.AuthenticationExceptionImpl;
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
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

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

        if (Boolean.FALSE.equals(authentication instanceof AnonymousAuthenticationToken)) {
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
        return Result.success(() -> blogService.findPage(currentPage, year));
    }

    @GetMapping("/secret/{blogId}")
    public Result<BlogExhibitVo> getLockedBlog(@PathVariable Long blogId,
                                               HttpServletRequest request) {

        String token = request.getHeader("Read-Token");
        boolean valid = blogService.checkToken(blogId, token);
        if (valid) {
            blogService.setReadCount(blogId);
            return Result.success(() -> blogService.findById(blogId, true));
        }
        throw new AuthenticationExceptionImpl("authorization exception");
    }

    @GetMapping("/token/{blogId}")
    public Result<Boolean> checkReadToken(@PathVariable Long blogId,
                                          HttpServletRequest request) {

        String token = request.getHeader("Read-Token");
        return Boolean.TRUE.equals(blogService.checkToken(blogId, token)) ? Result.success(true) : Result.success(false);
    }

    @GetMapping("/status/{blogId}")
    @Bloom(handler = DetailHandler.class)
    public Result<Integer> getBlogStatus(@PathVariable Long blogId) {

        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        Integer status = blogService.findStatusById(blogId);

        if (status == 0) {
            return Result.success(0);
        }

        if (Boolean.TRUE.equals(authentication instanceof AnonymousAuthenticationToken)) {
            return Result.success(1);
        }

        String authority = authentication.getAuthorities().stream()
                .findFirst()
                .map(GrantedAuthority::getAuthority)
                .orElseThrow();

        if (("ROLE_" + highestRole).equals(authority)) {
            return Result.success(0);
        }

        String userId = authentication.getName();
        return Result.success(() -> blogService.checkStatusByIdAndUserId(blogId, Long.valueOf(userId)));
    }

    @GetMapping("/years")
    public Result<List<Integer>> searchYears() {
        return Result.success(blogService::searchYears);
    }

    @GetMapping("/stat")
    public Result<VisitStatisticsVo> getVisitStatistics() {
        return Result.success(blogService::getVisitStatistics);
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
