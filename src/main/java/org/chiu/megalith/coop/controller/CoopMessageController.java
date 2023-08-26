package org.chiu.megalith.coop.controller;

import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.blog.vo.BlogEntityVo;
import org.chiu.megalith.coop.dto.impl.FinishCoopDto;
import org.chiu.megalith.coop.dto.impl.QuitCoopDto;
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

    @MessageMapping("/sync")
    public Result<Void> syncContent(@RequestBody SyncContentDto msg) {
        return Result.success(() -> coopMessageService.syncContent(msg));
    }

    @MessageMapping("/destroy")
    public Result<Void> finishCoop(@RequestBody FinishCoopDto msg) {
        return Result.success(() -> coopMessageService.destroySession(msg));
    }

    @MessageMapping("/quit")
    public Result<Void> quitEdit(@RequestBody QuitCoopDto msg) {
        return Result.success(() -> coopMessageService.quitEdit(msg));
    }

    @MessageMapping("/session/{userId}/{blogId}")
    public Result<Void> setUserToRedisSession(@DestinationVariable Long userId,
                                              @DestinationVariable Long blogId) {
        return Result.success(() -> coopMessageService.setUserToRedisSession(userId, blogId));
    }

    @MessageMapping("/blog/{blogId}")
    public Result<BlogEntityVo> getBlogContent(@DestinationVariable Long blogId) {
        return Result.success(() -> coopMessageService.getBlogContent(blogId));
    }

}
