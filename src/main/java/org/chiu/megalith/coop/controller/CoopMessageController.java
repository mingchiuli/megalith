package org.chiu.megalith.coop.controller;

import org.chiu.megalith.coop.req.FinishCoopReq;
import org.chiu.megalith.coop.req.QuitCoopReq;
import org.chiu.megalith.coop.req.SyncContentReq;
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
    public void syncContent(@RequestBody SyncContentReq msg) {
        coopMessageService.syncContent(msg);
    }

    @MessageMapping("/destroy")
    public void finishCoop(@RequestBody FinishCoopReq msg) {
        coopMessageService.destroySession(msg);
    }

    @MessageMapping("/quit")
    public void quitEdit(@RequestBody QuitCoopReq msg) {
        coopMessageService.quitEdit(msg);
    }

    @MessageMapping("/session/{userId}/{blogId}")
    public void setUserToRedisSession(@DestinationVariable Long userId,
                                      @DestinationVariable Long blogId) {
        coopMessageService.setUserToRedisSession(userId, blogId);
    }

    @MessageMapping("/blog/{blogId}")
    public void getBlogContent(@DestinationVariable Long blogId) {
        coopMessageService.getBlogContent(blogId);
    }

}
