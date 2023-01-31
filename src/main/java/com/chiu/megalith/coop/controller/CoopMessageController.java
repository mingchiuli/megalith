package com.chiu.megalith.coop.controller;

import com.chiu.megalith.coop.dto.impl.ChatDto;
import com.chiu.megalith.coop.dto.impl.DestroyDto;
import com.chiu.megalith.coop.dto.impl.QuitDto;
import com.chiu.megalith.coop.dto.impl.SyncContentDto;
import com.chiu.megalith.coop.service.CoopMessageService;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;


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
    public void chat(ChatDto.Bind msg) {
        coopMessageService.chat(msg);
    }

    @MessageMapping("/sync")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public void syncContent(SyncContentDto.Bind msg) {
        coopMessageService.syncContent(msg);
    }

    @MessageMapping("/destroy")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole())")
    public void destroy(DestroyDto.Bind msg) {
        coopMessageService.destroy(msg);
    }

    @MessageMapping("/quit")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public void quit(QuitDto.Bind msg) {
        coopMessageService.quit(msg);
    }

}
