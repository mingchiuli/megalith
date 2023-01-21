package com.chiu.megalith.ws.mq.handler.impl;

import com.chiu.megalith.ws.dto.Container;
import com.chiu.megalith.ws.dto.MessageDto;
import com.chiu.megalith.ws.dto.impl.SyncContentDto;
import com.chiu.megalith.ws.mq.handler.CoopHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class SyncContentHandler implements CoopHandler {
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
