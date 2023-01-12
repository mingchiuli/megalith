package com.chiu.megalith.websocket.controller;

import com.chiu.megalith.websocket.dto.impl.ChatInfoDto;
import com.chiu.megalith.websocket.dto.impl.DestroyDto;
import com.chiu.megalith.websocket.dto.impl.QuitDto;
import com.chiu.megalith.websocket.dto.impl.SyncContentDto;
import com.chiu.megalith.websocket.service.CoopMessageService;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

import java.security.Principal;

/**
 * @author mingchiuli
 * @create 2022-12-26 4:30 pm
 */
@Controller
@RequiredArgsConstructor
@RequestMapping("/coop")
public class CoopMessageController {

    private final CoopMessageService coopMessageService;


    @MessageMapping("/chat")
    public void chat(Principal user, ChatInfoDto.Message msg) {
        coopMessageService.chat(user, msg);
    }

    @MessageMapping("/sync")
    public void syncContent(Principal user, SyncContentDto.Content msg) {
        coopMessageService.sync(user, msg);
    }


    @MessageMapping("/destroy")
    public void destroy(Principal user, DestroyDto.Bind msg) {
        coopMessageService.destroy(user, msg);
    }

    @MessageMapping("/quit")
    public void quit(Principal user, QuitDto.Bind msg) {
        coopMessageService.quit(user, msg);
    }

}
