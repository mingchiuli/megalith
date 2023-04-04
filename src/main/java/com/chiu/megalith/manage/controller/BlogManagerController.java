package com.chiu.megalith.manage.controller;

import com.chiu.megalith.infra.exception.AuthenticationExceptionImpl;
import com.chiu.megalith.exhibit.dto.BlogEntityDto;
import com.chiu.megalith.exhibit.entity.BlogEntity;
import com.chiu.megalith.exhibit.service.BlogService;
import com.chiu.megalith.exhibit.vo.BlogExhibitVo;
import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.service.UserService;
import com.chiu.megalith.manage.vo.BlogEntityVo;
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

    private final UserService userService;

    @GetMapping("/info/authorize/{id}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<BlogExhibitVo> getLockedBlogDetail(@PathVariable(name = "id") Long id) {
        BlogEntity blog = blogService.findById(id);
        UserEntity user = userService.findById(blog.getUserId());
        blogService.setReadCount(id);
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

    @GetMapping("/info/echo/{id}")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public Result<BlogEntity> getEchoDetail(@PathVariable(name = "id") Long id) {
        Long userId = Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());
        BlogEntity blog = blogService.findById(id);
        if (!userId.equals(blog.getUserId())) {
            throw new AuthenticationExceptionImpl("must edit your blog!");
        }
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
        PageAdapter<BlogEntityDto> page = blogService.findAllABlogs(currentPage, size);
        return Result.success(page);
    }

    @GetMapping("/deleted")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public Result<PageAdapter<BlogEntity>> listDeletedBlogs(@RequestParam Integer currentPage,
                                                            @RequestParam Integer size) {
        PageAdapter<BlogEntity> deletedBlogs = blogService.findDeletedBlogs(currentPage, size);
        return Result.success(deletedBlogs);
    }

    @GetMapping("/recover/{id}/{idx}")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public Result<Void> recoverDeletedBlog(@PathVariable(value = "id") Long id, @PathVariable(value = "idx") Integer idx) {
        blogService.recoverDeletedBlog(id, idx);
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
