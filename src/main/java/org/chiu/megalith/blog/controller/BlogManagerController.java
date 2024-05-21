package org.chiu.megalith.blog.controller;

import jakarta.servlet.http.HttpServletResponse;
import org.chiu.megalith.blog.service.BlogManagerService;
import org.chiu.megalith.blog.vo.BlogDeleteVo;
import org.chiu.megalith.blog.vo.BlogEntityVo;
import org.chiu.megalith.blog.req.BlogEntityReq;
import org.chiu.megalith.infra.utils.SecurityUtils;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.infra.page.PageAdapter;

import lombok.RequiredArgsConstructor;

import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-01 9:28 pm
 */
@RestController
@RequiredArgsConstructor
@RequestMapping(value = "/sys/blog")
@Validated
public class BlogManagerController {

    private final BlogManagerService blogManagerService;

    @PostMapping("/save")
    @PreAuthorize("hasAuthority('sys:blog:save')")
    public Result<Void> saveOrUpdate(@RequestBody @Valid BlogEntityReq blog) {
        Long userId = SecurityUtils.getLoginUserId();
        return Result.success(() -> blogManagerService.saveOrUpdate(blog, userId));
    }

    @PostMapping("/delete")
    @PreAuthorize("hasAuthority('sys:blog:delete')")
    public Result<Void> deleteBlogs(@RequestBody @NotEmpty List<Long> ids) {
        String role = SecurityUtils.getLoginRole();
        Long userId = SecurityUtils.getLoginUserId();
        return Result.success(() -> blogManagerService.deleteBatch(ids, userId, role));
    }

    @GetMapping("/lock/{blogId}")
    @PreAuthorize("hasAuthority('sys:blog:lock')")
    public Result<String> setBlogToken(@PathVariable(value = "blogId") Long blogId) {
        return Result.success(() -> blogManagerService.setBlogToken(blogId));
    }

    @GetMapping("/blogs")
    @PreAuthorize("hasAuthority('sys:blog:blogs')")
    public Result<PageAdapter<BlogEntityVo>> getAllBlogs(@RequestParam(defaultValue = "1") Integer currentPage,
                                                         @RequestParam(defaultValue = "5") Integer size) {
        String role = SecurityUtils.getLoginRole();
        Long userId = SecurityUtils.getLoginUserId();
        return Result.success(() -> blogManagerService.findAllABlogs(currentPage, size, userId, role));
    }

    @GetMapping("/deleted")
    @PreAuthorize("hasAuthority('sys:blog:deleted')")
    public Result<PageAdapter<BlogDeleteVo>> getDeletedBlogs(@RequestParam Integer currentPage,
                                                             @RequestParam Integer size) {
        Long userId = SecurityUtils.getLoginUserId();
        return Result.success(() -> blogManagerService.findDeletedBlogs(currentPage, size, userId));
    }

    @GetMapping("/recover/{idx}")
    @PreAuthorize("hasAuthority('sys:blog:recover')")
    public Result<Void> recoverDeletedBlog(@PathVariable(value = "idx") Integer idx) {
        Long userId = SecurityUtils.getLoginUserId();
        return Result.success(() -> blogManagerService.recoverDeletedBlog(idx, userId));
    }

    @PostMapping("/oss/upload")
    @PreAuthorize("hasAuthority('sys:blog:oss:upload')")
    public Result<String> uploadOss(@RequestParam MultipartFile image) {
        Long userId = SecurityUtils.getLoginUserId();
        return Result.success(() -> blogManagerService.uploadOss(image, userId));
    }

    @GetMapping("/oss/delete")
    @PreAuthorize("hasAuthority('sys:blog:oss:delete')")
    public Result<Void> deleteOss(@RequestParam String url) {
        return Result.success(() -> blogManagerService.deleteOss(url));
    }

    @GetMapping("/download")
    @PreAuthorize("hasAuthority('sys:blog:download')")
    public Result<Void> download(HttpServletResponse response) {
        blogManagerService.download(response);
        return Result.success();
    }

}
