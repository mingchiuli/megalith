package org.chiu.megalith.blog.controller;

import org.chiu.megalith.blog.bloom.DetailHandler;
import org.chiu.megalith.blog.bloom.ListPageHandler;
import org.chiu.megalith.blog.vo.BlogDescriptionVo;
import org.chiu.megalith.blog.vo.VisitStatisticsVo;
import org.chiu.megalith.blog.bloom.Bloom;
import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.blog.vo.BlogExhibitVo;
import org.chiu.megalith.blog.vo.BlogHotReadVo;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.utils.SecurityUtils;
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

    @GetMapping("/info/{id}")
    @Bloom(handler = DetailHandler.class)
    public Result<BlogExhibitVo> getBlogDetail(@PathVariable(name = "id") Long id) {
        Authentication authentication = SecurityUtils.getLoginAuthentication();
        return Result.success(() -> blogService.getBlogDetail(authentication, id));
    }

    @GetMapping("/page/{currentPage}")
    @Bloom(handler = ListPageHandler.class)
    public Result<PageAdapter<BlogDescriptionVo>> getPage(@PathVariable(name = "currentPage") Integer currentPage,
                                                          @RequestParam(required = false, defaultValue = "-2147483648") Integer year) {
        return Result.success(() -> blogService.findPage(currentPage, year));
    }

    @GetMapping("/secret/{blogId}")
    @Bloom(handler = DetailHandler.class)
    public Result<BlogExhibitVo> getLockedBlog(@PathVariable Long blogId,
                                               @RequestParam(value = "readToken") String token) {
        return Result.success(blogService.getLockedBlog(blogId, token));
    }

    @GetMapping("/token/{blogId}")
    @Bloom(handler = DetailHandler.class)
    public Result<Boolean> checkReadToken(@PathVariable Long blogId,
                                          @RequestParam(value = "readToken") String token) {
        return Result.success(() -> blogService.checkToken(blogId, token));
    }

    @GetMapping("/status/{blogId}")
    @Bloom(handler = DetailHandler.class)
    public Result<Integer> getBlogStatus(@PathVariable Long blogId) {
        Authentication authentication = SecurityUtils.getLoginAuthentication();
        return Result.success(() -> blogService.getBlogStatus(authentication, blogId));
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
        return Result.success(blogService::getScoreBlogs);
    }

}
