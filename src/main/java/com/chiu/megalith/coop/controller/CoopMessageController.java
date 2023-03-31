package com.chiu.megalith.coop.controller;

import com.chiu.megalith.infra.lang.Result;
import com.chiu.megalith.coop.dto.impl.ChatDto;
import com.chiu.megalith.coop.dto.impl.DestroyDto;
import com.chiu.megalith.coop.dto.impl.QuitDto;
import com.chiu.megalith.coop.dto.impl.SyncDto;
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
    public Result<Void> chat(@RequestBody ChatDto.Bind msg) {
        coopMessageService.chat(msg);
        return Result.success();
    }

    @MessageMapping("/sync")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public void syncContent(@RequestBody SyncDto.Bind msg) {
        coopMessageService.syncContent(msg);
    }

    @MessageMapping("/destroy")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole())")
    public void destroy(@RequestBody DestroyDto.Bind msg) {
        coopMessageService.destroy(msg);
    }

    @MessageMapping("/quit")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public void quit(@RequestBody QuitDto.Bind msg) {
        coopMessageService.quit(msg);
    }

    @MessageMapping("/session/{userId}/{blogId}")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public void setUserToRedisSession(@DestinationVariable Long userId,
                                      @DestinationVariable Long blogId) {
        coopMessageService.setUserToRedisSession(userId, blogId);
    }

}
