package org.chiu.megalith.websocket.controller;

import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.manage.req.BlogEditPushAllReq;
import org.chiu.megalith.websocket.req.BlogEditPushActionReq;
import org.chiu.megalith.websocket.service.BlogMessageService;
import org.chiu.megalith.infra.utils.SecurityUtils;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequiredArgsConstructor
@RequestMapping("/edit")
@Validated
public class BlogMessageController {

    private final BlogMessageService blogMessageService;

    @MessageMapping("/push/action")
    @PreAuthorize("hasAuthority('sys:edit:push:action')")
    public void pushAction(@RequestBody @Valid BlogEditPushActionReq req) {
        Long userId = SecurityUtils.getLoginUserId();
        blogMessageService.pushAction(req, userId);
    }

    @PostMapping("/push/all")
    @PreAuthorize("hasAuthority('sys:blog:push:all')")
    @ResponseBody
    public Result<Void> pullSaveBlog(@RequestBody @Valid BlogEditPushAllReq blog) {
        Long userId = SecurityUtils.getLoginUserId();
        return Result.success(() -> blogMessageService.pushAll(blog, userId));
    }
}
