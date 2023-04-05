package com.chiu.megalith.coop.mq.handler;

import com.chiu.megalith.coop.dto.MessageDto;
import com.chiu.megalith.coop.dto.impl.ChatDto;
import com.chiu.megalith.coop.mq.BaseCoopHandler;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
public class ChatHandler extends BaseCoopHandler {

    public ChatHandler(SimpMessagingTemplate simpMessagingTemplate) {
        super(simpMessagingTemplate, "/chat");
    }

    @Override
    public boolean supports(MessageDto msg) {
        return msg instanceof ChatDto;
    }
}
