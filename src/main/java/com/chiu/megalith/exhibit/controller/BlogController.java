package com.chiu.megalith.exhibit.controller;

import com.chiu.megalith.infra.bloom.handler.impl.*;
import com.chiu.megalith.infra.exception.NotFoundException;
import com.chiu.megalith.infra.bloom.Bloom;
import com.chiu.megalith.infra.cache.Cache;
import com.chiu.megalith.exhibit.entity.BlogEntity;
import com.chiu.megalith.exhibit.service.BlogService;
import com.chiu.megalith.infra.lang.Const;
import com.chiu.megalith.infra.lang.Result;
import com.chiu.megalith.infra.page.PageAdapter;
import com.chiu.megalith.exhibit.vo.BlogExhibitVo;
import com.chiu.megalith.exhibit.vo.BlogHotReadVo;
import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.service.UserService;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
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
                ref.blog = blogService.findByIdAndInvisible(id);
            } else {
                ref.blog = blogService.findByIdAndVisible(id);
            }
        }, () -> ref.blog = blogService.findByIdAndVisible(id));

        blogService.setReadCount(id);
        return Result.success(ref.blog);
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
    @Cache(prefix = Const.HOT_BLOG)
    @Bloom(handler = CountYearHandler.class)
    public Result<Integer> getCountByYear(@PathVariable(name = "year") Integer year) {
        Integer count = blogService.getCountByYear(year);
        return Result.success(count);
    }

    @GetMapping("/token/{blogId}/{token}")
    public Result<BlogExhibitVo> getLockedBlog(@PathVariable Long blogId,
                                               @PathVariable String token) {
        BlogEntity blog = blogService.getLockedBlog(blogId, token);
        UserEntity user = userService.findById(blog.getUserId());
        blogService.setReadCount(blogId);
        return Result.success(
                BlogExhibitVo.builder()
                        .title(blog.getTitle())
                        .content(blog.getContent())
                        .readCount(blog.getReadCount())
                        .nickname(user.getNickname())
                        .avatar(user.getAvatar())
                        .created(blog.getCreated())
                        .readCount(blog.getReadCount())
                        .build()
        );
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
    public Result<Map<String, Long>> getVisitStatistics() {
        Map<String, Long> map = blogService.getVisitStatistics();
        return Result.success(map);
    }

    @GetMapping("/scores")
    public Result<List<BlogHotReadVo>> getScoreBlogs() {
        List<BlogHotReadVo> list = blogService.getScoreBlogs();
        list.forEach(item -> {
            String title;
            Long id = item.getId();
            try {
                title = blogService.findByIdAndVisible(id).getTitle();
            } catch (NotFoundException e) {
                title = blogService.findByIdAndInvisible(id).getTitle();
            }
            item.setTitle(title);
        });
        return Result.success(list);
    }

}
