package com.chiu.megalith.websocket.mq.handler.impl;

import com.chiu.megalith.websocket.dto.Container;
import com.chiu.megalith.websocket.dto.MessageDto;
import com.chiu.megalith.websocket.dto.impl.DestroyDto;
import com.chiu.megalith.websocket.mq.handler.WSHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;


@Component
@RequiredArgsConstructor
public class DestroyHandler implements WSHandler {

    private final SimpMessagingTemplate simpMessagingTemplate;


    @Override
    public boolean supports(MessageDto msg) {
        return msg instanceof DestroyDto;
    }

    @Override
    public void handle(MessageDto msg) {
        Container<DestroyDto.Bind> container = msg.getData();
        DestroyDto.Bind data = container.getData();
        Long blogId = data.getBlogId();
        simpMessagingTemplate.convertAndSendToUser(blogId.toString(),"/topic/destroy", data);
    }
}
