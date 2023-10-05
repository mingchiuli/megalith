package org.chiu.megalith.blog.controller;

import org.chiu.megalith.blog.vo.BlogDeleteVo;
import org.chiu.megalith.blog.vo.BlogEditVo;
import org.chiu.megalith.blog.vo.BlogEntityVo;
import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.manage.req.BlogEntityReq;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.infra.page.PageAdapter;

import lombok.RequiredArgsConstructor;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

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

    @Value("${blog.highest-role}")
    private String highestRole;

    @GetMapping("/echo/{id}")
    public Result<BlogEditVo> getEchoDetail(@PathVariable(name = "id") Long id) {
        Long userId = Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());
        return Result.success(() -> blogService.findEdit(id, userId));
    }

    @PostMapping("/save")
    public Result<Void> saveOrUpdate(@RequestBody @Validated BlogEntityReq blog) {
        Long userId = Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());
        blogService.saveOrUpdate(blog, userId);
        return Result.success();
    }

    @PostMapping("/delete")
    public Result<Void> deleteBlogs(@RequestBody List<Long> ids) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String authority = authentication.getAuthorities().stream()
                .findFirst()
                .map(GrantedAuthority::getAuthority)
                .orElseThrow();
        Long userId = Long.valueOf(authentication.getName());

        blogService.deleteBatch(ids, userId, authority);
        return Result.success();
    }

    @GetMapping("/lock/{blogId}")
    public Result<String> setBlogToken(@PathVariable(value = "blogId") Long blogId) {
        return Result.success(() -> blogService.setBlogToken(blogId));
    }

    @GetMapping("/blogs")
    public Result<PageAdapter<BlogEntityVo>> getAllBlogs(@RequestParam(defaultValue = "1") Integer currentPage,
                                                         @RequestParam(defaultValue = "5") Integer size) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        Long userId = Long.valueOf(authentication.getName());
        String authority = authentication.getAuthorities().stream()
                .findFirst()
                .map(GrantedAuthority::getAuthority)
                .orElseThrow();

        return Result.success(() -> blogService.findAllABlogs(currentPage, size, userId, authority));
    }

    @GetMapping("/deleted")
    public Result<PageAdapter<BlogDeleteVo>> getDeletedBlogs(@RequestParam Integer currentPage,
                                                             @RequestParam Integer size) {
        Long userId = Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());
        return Result.success(() -> blogService.findDeletedBlogs(currentPage, size, userId));
    }

    @GetMapping("/recover/{id}/{idx}")
    public Result<Void> recoverDeletedBlog(@PathVariable(value = "id") Long id,
                                           @PathVariable(value = "idx") Integer idx) {
        Long userId = Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());
        return Result.success(() -> blogService.recoverDeletedBlog(id, idx, userId));
    }

    @GetMapping("/status/{id}/{status}")
    public Result<Void> setBlogStatus(@PathVariable(value = "id") Long id,
                                      @PathVariable(value = "status") Integer status) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String authority = authentication.getAuthorities().stream()
                .findFirst()
                .map(GrantedAuthority::getAuthority)
                .orElseThrow();

        Long userId = Long.valueOf(authentication.getName());
        return Result.success(() -> blogService.setBlogStatus(id, userId, status, authority));
    }

    @PostMapping("/oss/upload")
    public Result<String> uploadOss(@RequestParam MultipartFile image) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String authority = authentication.getAuthorities().stream()
                .findFirst()
                .map(GrantedAuthority::getAuthority)
                .orElseThrow();

        Long userId = Long.valueOf(authentication.getName());
        return Result.success(() -> blogService.uploadOss(image, userId));
    }

    @GetMapping("/oss/delete")
    public Result<Void> deleteOss(@RequestParam String url) {
        return Result.success(() -> blogService.deleteOss(url));
    }
}
