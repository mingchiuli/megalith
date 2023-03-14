package com.chiu.megalith.coop.controller;

import com.chiu.megalith.coop.dto.*;
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
@MessageMapping("/coop")
public class CoopMessageController {

    private final CoopMessageService coopMessageService;

    @MessageMapping("/chat")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public void chat(@RequestBody MessageDto.BaseBind msg) {
        coopMessageService.chat(msg);
    }

    @MessageMapping("/sync")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public void syncContent(@RequestBody MessageDto.BaseBind msg) {
        coopMessageService.syncContent(msg);
    }

    @MessageMapping("/destroy")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole())")
    public void destroy(@RequestBody MessageDto.BaseBind msg) {
        coopMessageService.destroy(msg);
    }

    @MessageMapping("/quit")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public void quit(@RequestBody MessageDto.BaseBind msg) {
        coopMessageService.quit(msg);
    }

    @MessageMapping("/session/{userId}/{blogId}")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public void setUserToRedisSession(@DestinationVariable Long userId,
                              @DestinationVariable Long blogId) {
        coopMessageService.setUserToRedisSession(userId, blogId);
    }

}
