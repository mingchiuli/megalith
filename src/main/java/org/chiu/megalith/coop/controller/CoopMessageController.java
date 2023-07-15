package org.chiu.megalith.coop.controller;

import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.coop.dto.impl.ChatUserDto;
import org.chiu.megalith.coop.dto.impl.SubmitBlogDto;
import org.chiu.megalith.coop.dto.impl.QuitBlogDto;
import org.chiu.megalith.coop.dto.impl.SyncBlogDto;
import org.chiu.megalith.coop.service.CoopMessageService;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.handler.annotation.DestinationVariable;
import org.springframework.messaging.handler.annotation.MessageMapping;
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
    public Result<Void> chat(@RequestBody ChatUserDto msg) {
        coopMessageService.chatUser(msg);
        return Result.success();
    }

    @MessageMapping("/sync")
    public void syncBlog(@RequestBody SyncBlogDto msg) {
        coopMessageService.syncBlog(msg);
    }

    @MessageMapping("/save")
    public void submitBlog(@RequestBody SubmitBlogDto msg) {
        coopMessageService.submitBlog(msg);
    }

    @MessageMapping("/quit")
    public void quitBlog(@RequestBody QuitBlogDto msg) {
        coopMessageService.quitBlog(msg);
    }

    @MessageMapping("/session/{userId}/{blogId}")
    public void setUserToRedisSession(@DestinationVariable Long userId,
                                      @DestinationVariable Long blogId) {
        coopMessageService.setUserToRedisSession(userId, blogId);
    }

}
