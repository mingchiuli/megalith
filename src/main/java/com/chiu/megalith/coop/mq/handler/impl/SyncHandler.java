package com.chiu.megalith.coop.mq.handler.impl;

import com.chiu.megalith.coop.dto.Container;
import com.chiu.megalith.coop.dto.MessageDto;
import com.chiu.megalith.coop.dto.impl.SyncDto;
import com.chiu.megalith.coop.mq.handler.CoopHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class SyncHandler implements CoopHandler {
    private final SimpMessagingTemplate simpMessagingTemplate;

    @Override
    public boolean supports(MessageDto msg) {
        return msg instanceof SyncDto;
    }

    @Override
    public void handle(MessageDto msg) {
        Container<SyncDto.Bind> container = msg.getData();
        SyncDto.Bind data = container.getData();
        Long id = data.getBlogId();
        Long to = data.getToOne();
        simpMessagingTemplate.convertAndSendToUser(to.toString(), "/" + id + "/topic/content", data);
    }
}
