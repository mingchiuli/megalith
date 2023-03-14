package com.chiu.megalith.coop.mq.handler.impl;

import com.chiu.megalith.coop.dto.MessageDto;
import com.chiu.megalith.coop.dto.ChatDto;
import com.chiu.megalith.coop.mq.handler.CoopHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class ChatHandler implements CoopHandler {

    private final SimpMessagingTemplate simpMessagingTemplate;

    @Override
    public boolean supports(MessageDto msg) {
        return msg instanceof ChatDto;
    }

    @Override
    public void handle(MessageDto msg) {
        MessageDto.Container<MessageDto.BaseBind> container = msg.getData();
        MessageDto.BaseBind data = container.getData();
        Long id = data.getBlogId();
        Long to = data.getToOne();
        simpMessagingTemplate.convertAndSendToUser(to.toString(), "/" + id + "/chat", data);
    }
}
