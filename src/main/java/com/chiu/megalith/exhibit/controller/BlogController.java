package com.chiu.megalith.exhibit.controller;

import com.chiu.megalith.exhibit.bloom.Bloom;
import com.chiu.megalith.exhibit.bloom.handler.impl.*;
import com.chiu.megalith.exhibit.cache.Cache;
import com.chiu.megalith.exhibit.entity.BlogEntity;
import com.chiu.megalith.exhibit.service.BlogService;
import com.chiu.megalith.base.lang.Const;
import com.chiu.megalith.base.lang.Result;
import com.chiu.megalith.base.page.PageAdapter;
import com.chiu.megalith.exhibit.vo.BlogExhibitVo;
import com.chiu.megalith.manage.service.UserService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-11-26 5:30 pm
 */
@RestController
@RequestMapping(value = "/public/blog")
@RequiredArgsConstructor
public class BlogController {

    private final BlogService blogService;

    private final UserService userService;

    @GetMapping("/info/{id}")
    @Bloom(handler = DetailPageHandler.class)
    public Result<BlogExhibitVo> getBlogDetail(@PathVariable(name = "id") Long id) {
        BlogExhibitVo blog = blogService.findByIdAndStatus(id, 0);
        blogService.setReadCount(id);
        return Result.success(blog);
    }

    @GetMapping("/page/total/{currentPage}")
    @Cache(prefix = Const.HOT_BLOGS)
    @Bloom(handler = ListPageHandler.class)
    public Result<PageAdapter<BlogEntity>> listPage(@PathVariable(name = "currentPage") Integer currentPage) {
        PageAdapter<BlogEntity> pageData = blogService.findPage(currentPage);
        return Result.success(pageData);
    }

    @GetMapping("/page/year/{year}/{currentPage}")
    @Cache(prefix = Const.HOT_BLOGS)
    @Bloom(handler = ListByYearPageHandler.class)
    public Result<PageAdapter<BlogEntity>> listPageByYear(@PathVariable(name = "currentPage") Integer currentPage,
                                                          @PathVariable(name = "year") Integer year) {
        PageAdapter<BlogEntity> pageData = blogService.findPageByYear(currentPage, year);
        return Result.success(pageData);
    }

    @GetMapping("/count/year/{year}")
    @Cache(prefix = Const.HOT_BLOGS)
    @Bloom(handler = CountYearHandler.class)
    public Result<Integer> getCountByYear(@PathVariable(name = "year") Integer year) {
        Integer count = blogService.getCountByYear(year);
        return Result.success(count);
    }

    @GetMapping("/token/{blogId}/{token}")
    public Result<BlogExhibitVo> getLockedBlog(@PathVariable Long blogId,
                                            @PathVariable String token) {
        BlogEntity blog = blogService.getLockedBlog(blogId, token);
        Optional<String> nickname = userService.findNicknameById(blog.getUserId());
        blogService.setReadCount(blogId);
        return Result.success(
                BlogExhibitVo
                        .builder()
                        .title(blog.getTitle())
                        .content(blog.getContent())
                        .readCount(blog.getReadCount())
                        .nickname(nickname.orElse("anonymous"))
                        .created(blog.getCreated())
                        .readCount(blog.getReadCount())
                        .build()
        );
    }

    @GetMapping("/status/{blogId}")
    @Bloom(handler = BlogStatusHandler.class)
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

}
