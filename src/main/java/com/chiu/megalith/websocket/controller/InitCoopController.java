package com.chiu.megalith.websocket.controller;

import com.chiu.megalith.common.lang.Result;
import com.chiu.megalith.common.valid.CoopBlogId;
import com.chiu.megalith.websocket.service.InitCoopService;
import com.chiu.megalith.websocket.vo.InitCoopVo;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * @author mingchiuli
 * @create 2022-12-25 7:14 pm
 */
@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("/room")
public class InitCoopController {

    private final InitCoopService initCoopService;

    @GetMapping("/init/{blogId}/{orderNumber}")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public Result<InitCoopVo> initCoop(@PathVariable @CoopBlogId Long blogId, @PathVariable Integer orderNumber) {
        InitCoopVo initCoopVo = initCoopService.initCoop(blogId, orderNumber);
        return Result.success(initCoopVo);
    }
}
