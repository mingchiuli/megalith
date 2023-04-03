package com.chiu.megalith.coop.controller;

import com.chiu.megalith.coop.vo.BlogAbstractVo;
import com.chiu.megalith.infra.page.PageAdapter;
import com.chiu.megalith.manage.vo.BlogEntityVo;
import com.chiu.megalith.infra.lang.Result;
import com.chiu.megalith.infra.valid.CoopBlogId;
import com.chiu.megalith.coop.service.CoopService;
import com.chiu.megalith.coop.vo.InitCoopVo;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;


/**
 * @author mingchiuli
 * @create 2022-12-25 7:14 pm
 */
@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("/coop")
public class CoopController {
    private final CoopService coopService;

    @GetMapping("/init/{blogId}/{orderNumber}")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public Result<InitCoopVo> joinCoopBlog(@PathVariable @CoopBlogId Long blogId,
                                       @PathVariable Integer orderNumber) {
        InitCoopVo initCoopVo = coopService.joinCoopBlog(blogId, orderNumber);
        return Result.success(initCoopVo);
    }

    @GetMapping("/blogs/{currentPage}")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public Result<PageAdapter<BlogAbstractVo>> getCoopBlogs(@PathVariable Integer currentPage) {
        PageAdapter<BlogAbstractVo> page = coopService.getCoopBlogs(currentPage);
        return Result.success(page);
    }

    @PostMapping("/submit/{blogId}")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public Result<Void> submitBlog(@PathVariable @CoopBlogId Long blogId,
                               @RequestBody @Validated BlogEntityVo blogEntityVo) {
        coopService.submitBlog(blogId, blogEntityVo);
        return Result.success();
    }
}
