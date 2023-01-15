package com.chiu.megalith.websocket.mq.handler.impl;

import com.chiu.megalith.websocket.dto.Container;
import com.chiu.megalith.websocket.dto.MessageDto;
import com.chiu.megalith.websocket.dto.impl.QuitDto;
import com.chiu.megalith.websocket.mq.handler.WSHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

/**
 * @author mingchiuli
 * @create 2022-12-30 11:55 pm
 */
@Component
@RequiredArgsConstructor
public class QuitHandler implements WSHandler {

    private final SimpMessagingTemplate simpMessagingTemplate;


    @Override
    public boolean supports(MessageDto msg) {
        return msg instanceof QuitDto;
    }

    @Override
    public void handle(MessageDto msg) {
        Container<QuitDto.Bind> container = msg.getData();
        QuitDto.Bind data = container.getData();
        Long blogId = data.getBlogId();
        simpMessagingTemplate.convertAndSendToUser(blogId.toString(),"/topic/destroy", data);
    }
}
