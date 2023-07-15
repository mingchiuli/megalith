package org.chiu.megalith.coop.mq.handler;

import org.chiu.megalith.coop.dto.BaseDto;
import org.chiu.megalith.coop.dto.impl.ChatUserDto;
import org.chiu.megalith.coop.mq.BaseCoopHandler;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
public class ChatUserHandler extends BaseCoopHandler {

    public ChatUserHandler(SimpMessagingTemplate simpMessagingTemplate) {
        super(simpMessagingTemplate, "/chat");
    }

    @Override
    public boolean supports(BaseDto msg) {
        return msg instanceof ChatUserDto;
    }
}
