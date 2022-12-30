package com.chiu.megalith.websocket.handler.impl;

import com.chiu.megalith.websocket.dto.Container;
import com.chiu.megalith.websocket.dto.MessageDto;
import com.chiu.megalith.websocket.dto.impl.SyncContentDto;
import com.chiu.megalith.websocket.handler.WSHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class SyncContentHandler implements WSHandler {
    private final SimpMessagingTemplate simpMessagingTemplate;

    @Override
    public boolean supports(MessageDto msg) {
        return msg instanceof SyncContentDto;
    }

    @Override
    public void handle(MessageDto msg) {
        Container<SyncContentDto.Content> container = msg.getData();
        SyncContentDto.Content content = container.getData();
        simpMessagingTemplate.convertAndSend("/topic/content/" + content.getBlogId(), content);
    }
}
