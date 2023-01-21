package com.chiu.megalith.ws.mq.handler.impl;

import com.chiu.megalith.ws.dto.Container;
import com.chiu.megalith.ws.dto.MessageDto;
import com.chiu.megalith.ws.dto.impl.DestroyDto;
import com.chiu.megalith.ws.mq.handler.CoopHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DestroyHandler implements CoopHandler {

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
        simpMessagingTemplate.convertAndSend("/" + blogId + "/topic/destroy", data);
    }
}
