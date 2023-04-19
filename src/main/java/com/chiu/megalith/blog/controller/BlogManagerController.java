package com.chiu.megalith.blog.controller;

import com.chiu.megalith.infra.exception.AuthenticationExceptionImpl;
import com.chiu.megalith.blog.dto.BlogEntityDto;
import com.chiu.megalith.blog.entity.BlogEntity;
import com.chiu.megalith.blog.service.BlogService;
import com.chiu.megalith.blog.vo.BlogEntityVo;
import com.chiu.megalith.infra.lang.Result;
import com.chiu.megalith.infra.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-01 9:28 pm
 */
@RestController
@RequiredArgsConstructor
@RequestMapping(value = "/sys/blog")
public class BlogManagerController {

    private final BlogService blogService;

    @GetMapping("/info/echo/{id}")
    public Result<BlogEntity> getEchoDetail(@PathVariable(name = "id") Long id) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());
        BlogEntity blog = blogService.findById(id);
        if (!blog.getUserId().equals(userId)) {
            throw new AuthenticationExceptionImpl("must edit your blog!");
        }
        return Result.success(blog);
    }

    @PostMapping("/save")
    public Result<Void> saveOrUpdate(@RequestBody @Validated BlogEntityVo blog) {
        blogService.saveOrUpdate(blog);
        return Result.success();
    }

    @PostMapping("/delete")
    public Result<Void> deleteBlogs(@RequestBody List<Long> ids) {
        blogService.deleteBlogs(ids);
        return Result.success();
    }

    @GetMapping("/set/token")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> setBlogToken() {
        blogService.setBlogToken();
        return Result.success();
    }

    @GetMapping("/get/token")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<String> getBlogToken() {
        String token = blogService.getBlogToken();
        return Result.success(token);
    }

    @GetMapping("/blogs")
    public Result<PageAdapter<BlogEntityDto>> getAllBlogs(@RequestParam(defaultValue = "1") Integer currentPage,
                                                          @RequestParam(defaultValue = "5") Integer size) {
        PageAdapter<BlogEntityDto> page = blogService.findAllABlogs(currentPage, size);
        return Result.success(page);
    }

    @GetMapping("/deleted")
    public Result<PageAdapter<BlogEntity>> listDeletedBlogs(@RequestParam Integer currentPage,
                                                            @RequestParam Integer size) {
        PageAdapter<BlogEntity> deletedBlogs = blogService.findDeletedBlogs(currentPage, size);
        return Result.success(deletedBlogs);
    }

    @GetMapping("/recover/{id}/{idx}")
    public Result<Void> recoverDeletedBlog(@PathVariable(value = "id") Long id,
                                           @PathVariable(value = "idx") Integer idx) {
        blogService.recoverDeletedBlog(id, idx);
        return Result.success();
    }

    @GetMapping("/status/{id}/{status}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> changeBlogStatus(@PathVariable(value = "id") Long id,
                                         @PathVariable(value = "status") Integer status) {
        blogService.changeBlogStatus(id, status);
        return Result.success();
    }
}
