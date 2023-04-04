package com.chiu.megalith.coop.controller;

import com.chiu.megalith.infra.lang.Result;
import com.chiu.megalith.coop.dto.impl.ChatDto;
import com.chiu.megalith.coop.dto.impl.SubmitBlogDto;
import com.chiu.megalith.coop.dto.impl.QuitBlogDto;
import com.chiu.megalith.coop.dto.impl.SyncBlogDto;
import com.chiu.megalith.coop.service.CoopMessageService;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.handler.annotation.DestinationVariable;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;


/**
 * @author mingchiuli
 * @create 2022-12-26 4:30 pm
 */
@Controller
@RequiredArgsConstructor
@MessageMapping("/message")
public class CoopMessageController {

    private final CoopMessageService coopMessageService;

    @MessageMapping("/chat")
    public Result<Void> chat(@RequestBody ChatDto.Bind msg) {
        coopMessageService.chat(msg);
        return Result.success();
    }

    @MessageMapping("/sync")
    public void syncBlog(@RequestBody SyncBlogDto.Bind msg) {
        coopMessageService.syncBlog(msg);
    }

    @MessageMapping("/submit")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public void submitBlog(@RequestBody SubmitBlogDto.Bind msg) {
        coopMessageService.submitBlog(msg);
    }

    @MessageMapping("/quit")
    public void quitBlog(@RequestBody QuitBlogDto.Bind msg) {
        coopMessageService.quitBlog(msg);
    }

    @MessageMapping("/session/{userId}/{blogId}")
    public void setUserToRedisSession(@DestinationVariable Long userId,
                                      @DestinationVariable Long blogId) {
        coopMessageService.setUserToRedisSession(userId, blogId);
    }

}
