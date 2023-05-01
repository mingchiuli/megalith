package com.chiu.megalith.blog.controller;

import com.chiu.megalith.blog.vo.BlogDescriptionVo;
import com.chiu.megalith.blog.vo.VisitStatisticsVo;
import com.chiu.megalith.infra.bloom.handler.impl.*;
import com.chiu.megalith.infra.exception.NotFoundException;
import com.chiu.megalith.infra.bloom.Bloom;
import com.chiu.megalith.infra.cache.Cache;
import com.chiu.megalith.blog.service.BlogService;
import com.chiu.megalith.infra.lang.Const;
import com.chiu.megalith.infra.lang.Result;
import com.chiu.megalith.infra.page.PageAdapter;
import com.chiu.megalith.blog.vo.BlogExhibitVo;
import com.chiu.megalith.blog.vo.BlogHotReadVo;
import jakarta.validation.constraints.NotBlank;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

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
        var ref = new Object() {
            BlogExhibitVo blog;
        };

        Optional.ofNullable(SecurityContextHolder.getContext().getAuthentication()).ifPresentOrElse(authentication -> {
            String authority = authentication.getAuthorities().stream()
                    .findFirst()
                    .map(GrantedAuthority::getAuthority)
                    .orElseThrow();

            if (("ROLE_" + highestRole).equals(authority)) {
                ref.blog = blogService.findById(id, true);
            } else {
                ref.blog = blogService.findById(id, false);
            }
        }, () -> ref.blog = blogService.findById(id, false));

        blogService.setReadCount(id);
        return Result.success(ref.blog);
    }

    @GetMapping("/page/{currentPage}")
    @Bloom(handler = ListPageHandler.class)
    public Result<PageAdapter<BlogDescriptionVo>> listPage(@PathVariable(name = "currentPage") Integer currentPage,
                                                           @RequestParam(required = false, defaultValue = "-2147483648") Integer year) {
        PageAdapter<BlogDescriptionVo> page = blogService.findPage(currentPage, year);
        if (!Objects.equals(year, Integer.MIN_VALUE)) {
            Integer count = blogService.getCountByYear(year);
            page.setAdditional(count);
        }
        return Result.success(page);
    }

    @GetMapping("/token/{blogId}")
    public Result<BlogExhibitVo> getLockedBlog(@PathVariable Long blogId,
                                               @RequestParam @NotBlank String token) {
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
    @Cache(prefix = Const.BLOG_STATUS)
    public Result<Integer> getBlogStatus(@PathVariable Long blogId) {
        Integer status = blogService.findStatusById(blogId);
        return Result.success(status);
    }

    @GetMapping("/years")
    @Cache(prefix = Const.YEARS)
    public Result<List<Integer>> searchYears() {
        List<Integer> years = blogService.searchYears();
        return Result.success(years);
    }

    @GetMapping("/statistic")
    public Result<VisitStatisticsVo> getVisitStatistics() {
        VisitStatisticsVo vo = blogService.getVisitStatistics();
        return Result.success(vo);
    }

    @GetMapping("/scores")
    public Result<List<BlogHotReadVo>> getScoreBlogs() {
        List<BlogHotReadVo> list = blogService.getScoreBlogs();
        list.forEach(item -> {
            String title;
            Long id = item.getId();
            try {
                title = blogService.findById(id, false).getTitle();
            } catch (NotFoundException e) {
                title = blogService.findById(id, true).getTitle();
            }
            item.setTitle(title);
        });
        return Result.success(list);
    }

}
