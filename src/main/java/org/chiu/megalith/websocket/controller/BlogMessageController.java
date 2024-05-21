package org.chiu.megalith.websocket.controller;

import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.websocket.req.BlogEditPushActionReq;
import org.chiu.megalith.websocket.req.BlogEditPushAllReq;
import org.chiu.megalith.websocket.service.BlogMessageService;
import org.chiu.megalith.websocket.vo.BlogEditVo;
import org.chiu.megalith.infra.utils.SecurityUtils;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
@Validated
public class BlogMessageController {

    private final BlogMessageService blogMessageService;

    @MessageMapping("/edit/push/action")
    @PreAuthorize("hasAuthority('sys:edit:push:action')")
    public void pushAction(@RequestBody @Valid BlogEditPushActionReq req) {
        Long userId = SecurityUtils.getLoginUserId();
        blogMessageService.pushAction(req, userId);
    }

    @PostMapping("/edit/push/all")
    @PreAuthorize("hasAuthority('sys:blog:push:all')")
    public Result<Void> pullSaveBlog(@RequestBody @Valid BlogEditPushAllReq blog) {
        Long userId = SecurityUtils.getLoginUserId();
        return Result.success(() -> blogMessageService.pushAll(blog, userId));
    }

    @GetMapping("/edit/pull/echo")
    @PreAuthorize("hasAuthority('sys:blog:echo')")
    public Result<BlogEditVo> getEchoDetail(@RequestParam(value = "blogId", required = false) Long id) {
        Long userId = SecurityUtils.getLoginUserId();
        return Result.success(() -> blogMessageService.findEdit(id, userId));
    }
}
