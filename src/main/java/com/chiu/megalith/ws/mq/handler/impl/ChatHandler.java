package com.chiu.megalith.ws.mq.handler.impl;

import com.chiu.megalith.ws.dto.Container;
import com.chiu.megalith.ws.dto.MessageDto;
import com.chiu.megalith.ws.dto.impl.ChatInfoDto;
import com.chiu.megalith.ws.mq.handler.CoopHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class ChatHandler implements CoopHandler {

    private final SimpMessagingTemplate simpMessagingTemplate;

    @Override
    public boolean supports(MessageDto msg) {
        return msg instanceof ChatInfoDto;
    }

    @Override
    public void handle(MessageDto msg) {
        Container<ChatInfoDto.Bind> container = msg.getData();
        ChatInfoDto.Bind data = container.getData();
        data.setToAll(null);
        Long id = data.getBlogId();
        Long to = data.getToOne();
        simpMessagingTemplate.convertAndSendToUser(to.toString(), "/" + id + "/queue/chat", data);
    }
}
