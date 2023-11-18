package org.chiu.megalith.blog.controller;

import org.chiu.megalith.blog.req.BlogEditPushActionReq;
import org.chiu.megalith.blog.service.BlogMessageService;
import org.chiu.megalith.infra.utils.SecurityUtils;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;

import lombok.RequiredArgsConstructor;

@Controller
@MessageMapping("/edit")
@RequiredArgsConstructor
public class BlogMessageController {

    private final BlogMessageService blogMessageService;


    @MessageMapping("/push/action")
    public void pushAction(@RequestBody BlogEditPushActionReq req) {
        Long userId = SecurityUtils.getLoginUserId();
        blogMessageService.pushAction(req, userId);
    }
}
