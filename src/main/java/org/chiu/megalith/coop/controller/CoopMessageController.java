package org.chiu.megalith.coop.controller;

import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.coop.dto.impl.UserChatDto;
import org.chiu.megalith.coop.dto.impl.SubmitBlogDto;
import org.chiu.megalith.coop.dto.impl.QuitBlogDto;
import org.chiu.megalith.coop.dto.impl.SyncContentDto;
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
    public Result<Void> chat(@RequestBody UserChatDto msg) {
        return Result.success(() -> coopMessageService.chatUser(msg));
    }

    @MessageMapping("/sync")
    public Result<Void> syncBlog(@RequestBody SyncContentDto msg) {
        return Result.success(() -> coopMessageService.syncContent(msg));
    }

    @MessageMapping("/save")
    public Result<Void> submitBlog(@RequestBody SubmitBlogDto msg) {
        return Result.success(() -> coopMessageService.submitBlog(msg));
    }

    @MessageMapping("/quit")
    public Result<Void> quitBlog(@RequestBody QuitBlogDto msg) {
        return Result.success(() -> coopMessageService.quitEdit(msg));
    }

    @MessageMapping("/session/{userId}/{blogId}")
    public Result<Void> setUserToRedisSession(@DestinationVariable Long userId,
                                              @DestinationVariable Long blogId) {
        return Result.success(() -> coopMessageService.setUserToRedisSession(userId, blogId));
    }

}
