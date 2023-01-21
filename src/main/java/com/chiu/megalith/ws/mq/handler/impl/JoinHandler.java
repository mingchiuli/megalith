package com.chiu.megalith.ws.mq.handler.impl;

import com.chiu.megalith.ws.dto.Container;
import com.chiu.megalith.ws.dto.MessageDto;
import com.chiu.megalith.ws.dto.impl.JoinDto;
import com.chiu.megalith.ws.mq.handler.CoopHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

/**
 * @author mingchiuli
 * @create 2023-01-21 4:59 pm
 */
@Component
@RequiredArgsConstructor
public class JoinHandler implements CoopHandler {

    private final SimpMessagingTemplate simpMessagingTemplate;

    @Override
    public boolean supports(MessageDto msg) {
        return msg instanceof JoinDto;
    }

    @Override
    public void handle(MessageDto msg) {
        Container<JoinDto.Bind> container = msg.getData();
        JoinDto.Bind data = container.getData();
        Long blogId = data.getBlogId();
        simpMessagingTemplate.convertAndSendToUser(blogId.toString(),"/topic/join", data);
    }
}
