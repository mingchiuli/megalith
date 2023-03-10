package com.chiu.megalith.coop.controller;

import com.chiu.megalith.manage.vo.BlogEntityVo;
import com.chiu.megalith.base.lang.Result;
import com.chiu.megalith.base.valid.CoopBlogId;
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
    public Result<InitCoopVo> joinCoop(@PathVariable @CoopBlogId Long blogId,
                                       @PathVariable Integer orderNumber) {
        InitCoopVo initCoopVo = coopService.joinCoop(blogId, orderNumber);
        return Result.success(initCoopVo);
    }

    @PostMapping("/submit/{blogId}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> submit(@PathVariable @CoopBlogId Long blogId,
                               @RequestBody @Validated BlogEntityVo blogEntityVo) {
        coopService.submit(blogId, blogEntityVo);
        return Result.success();
    }
}
