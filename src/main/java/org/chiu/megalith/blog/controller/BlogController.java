package org.chiu.megalith.blog.controller;

import org.chiu.megalith.blog.vo.BlogDescriptionVo;
import org.chiu.megalith.blog.vo.VisitStatisticsVo;
import org.chiu.megalith.infra.bloom.handler.impl.*;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.bloom.Bloom;
import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.blog.vo.BlogExhibitVo;
import org.chiu.megalith.blog.vo.BlogHotReadVo;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.utils.SecurityUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
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
        Authentication authentication = SecurityUtils.getLoginAuthentication();
        BlogExhibitVo blog;

        if (Boolean.FALSE.equals(authentication instanceof AnonymousAuthenticationToken)) {
            if ((Const.ROLE_PREFIX.getInfo() + highestRole).equals(SecurityUtils.getLoginAuthority())) {
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
                                               @RequestParam(value = "readToken") String token) {

        boolean valid = blogService.checkToken(blogId, token);
        if (valid) {
            blogService.setReadCount(blogId);
            return Result.success(() -> blogService.findById(blogId, true));
        }
        throw new BadCredentialsException("authorization exception");
    }

    @GetMapping("/token/{blogId}")
    public Result<Boolean> checkReadToken(@PathVariable Long blogId,
                                          @RequestParam(value = "readToken") String token) {

        return Boolean.TRUE.equals(blogService.checkToken(blogId, token)) ?
                Result.success(true) : 
                Result.success(false);
    }

    @GetMapping("/status/{blogId}")
    @Bloom(handler = DetailHandler.class)
    public Result<Integer> getBlogStatus(@PathVariable Long blogId) {

        Authentication authentication = SecurityUtils.getLoginAuthentication();
        Integer status = blogService.findStatusById(blogId);

        if (StatusEnum.NORMAL.getCode().equals(status)) {
            return Result.success(StatusEnum.NORMAL.getCode());
        }

        if (Boolean.TRUE.equals(authentication instanceof AnonymousAuthenticationToken)) {
            return Result.success(StatusEnum.HIDE.getCode());
        }

        if (("ROLE_" + highestRole).equals(SecurityUtils.getLoginAuthority())) {
            return Result.success(StatusEnum.NORMAL.getCode());
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
            } catch (MissException e) {
                title = blogService.findById(id, true).getTitle();
            }
            item.setTitle(title);
        });
        return Result.success(hotList);
    }

}
