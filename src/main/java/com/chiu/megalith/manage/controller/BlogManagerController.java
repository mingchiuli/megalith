package com.chiu.megalith.manage.controller;

import com.chiu.megalith.exhibit.dto.BlogEntityDto;
import com.chiu.megalith.exhibit.entity.BlogEntity;
import com.chiu.megalith.exhibit.service.BlogService;
import com.chiu.megalith.manage.vo.BlogEntityVo;
import com.chiu.megalith.common.lang.Result;
import com.chiu.megalith.common.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
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

    @GetMapping("/info/authorize/{id}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<BlogEntity> getLockedBlogDetail(@PathVariable(name = "id") Long id) {
        BlogEntity blog = blogService.findById(id);
        blogService.setReadCount(id);
        return Result.success(blog);
    }

    @PostMapping("/save")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public Result<Void> saveOrUpdate(@RequestBody @Validated BlogEntityVo blog) {
        blogService.saveOrUpdate(blog);
        return Result.success();
    }

    @PostMapping("/delete")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
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
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public Result<PageAdapter<BlogEntityDto>> getAllBlogs(@RequestParam(defaultValue = "1") Integer currentPage,
                                                          @RequestParam(defaultValue = "5") Integer size) {
        PageAdapter<BlogEntityDto> page = blogService.getAllABlogs(currentPage, size);
        return Result.success(page);
    }

    @GetMapping("/deleted")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public Result<PageAdapter<BlogEntity>> listDeletedBlogs(@RequestParam Integer currentPage,
                                                            @RequestParam Integer size) {
        PageAdapter<BlogEntity> deletedBlogs = blogService.listDeletedBlogs(currentPage, size);
        return Result.success(deletedBlogs);
    }

    @GetMapping("/recover/{id}")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public Result<Void> recoverDeletedBlog(@PathVariable(value = "id") Long id) {
        blogService.recoverDeletedBlog(id);
        return Result.success();
    }

    @GetMapping("/status/{id}/{status}/{year}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> changeBlogStatus(@PathVariable(value = "id") Long id,
                                         @PathVariable(value = "status") Integer status,
                                         @PathVariable(value = "year") Integer year) {
        blogService.changeBlogStatus(id, status, year);
        return Result.success();
    }
}
