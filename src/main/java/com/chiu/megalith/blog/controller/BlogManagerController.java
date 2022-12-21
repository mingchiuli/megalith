package com.chiu.megalith.blog.controller;

import com.chiu.megalith.blog.dto.BlogEntityDto;
import com.chiu.megalith.blog.service.BlogService;
import com.chiu.megalith.blog.vo.BlogEntityVo;
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

    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    @PostMapping("/edit")
    public Result<Void> saveOrUpdate(@RequestBody @Validated BlogEntityVo blog) {
        blogService.saveOrUpdate(blog);
        return Result.success();
    }

    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    @PostMapping("/delete")
    public Result<Void> deleteBlogs(@RequestBody List<Long> ids) {
        blogService.deleteBlogs(ids);
        return Result.success();
    }

    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    @GetMapping("/set/token")
    public Result<Void> setBlogToken() {
        blogService.setBlogToken();
        return Result.success();
    }

    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    @GetMapping("/get/token")
    public Result<String> getBlogToken() {
        String token = blogService.getBlogToken();
        return Result.success(token);
    }

    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    @GetMapping("/get/blogs")
    public Result<PageAdapter<BlogEntityDto>> getAllBlogs(@RequestParam(defaultValue = "1") Integer currentPage, @RequestParam(defaultValue = "5") Integer size) {
        PageAdapter<BlogEntityDto> page = blogService.getAllABlogs(currentPage, size);
        return Result.success(page);
    }
}
