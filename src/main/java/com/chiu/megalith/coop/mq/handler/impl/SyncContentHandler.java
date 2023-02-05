package com.chiu.megalith.coop.mq.handler.impl;

import com.chiu.megalith.coop.dto.Container;
import com.chiu.megalith.coop.dto.MessageDto;
import com.chiu.megalith.coop.dto.impl.SyncContentDto;
import com.chiu.megalith.coop.mq.handler.CoopHandler;
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
        Container<SyncContentDto.Bind> container = msg.getData();
        SyncContentDto.Bind content = container.getData();
        content.getTos().forEach(id ->
                simpMessagingTemplate.convertAndSendToUser(id.toString(),"/topic/content/" + content.getBlogId(), content));
    }
}