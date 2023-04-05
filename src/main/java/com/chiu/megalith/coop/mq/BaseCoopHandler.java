package com.chiu.megalith.coop.mq;

import com.chiu.megalith.coop.dto.MessageDto;
import org.springframework.messaging.simp.SimpMessagingTemplate;

public abstract class BaseCoopHandler {

    private final SimpMessagingTemplate simpMessagingTemplate;

    protected final String uri;

    public BaseCoopHandler(SimpMessagingTemplate simpMessagingTemplate,
                           String uri) {
        this.simpMessagingTemplate = simpMessagingTemplate;
        this.uri = uri;
    }

    public abstract boolean supports(MessageDto msg);

    public void handle(MessageDto msg) {
        MessageDto.Container<MessageDto.BaseBind> container = msg.getData();
        MessageDto.BaseBind data = container.getData();
        Long id = data.getBlogId();
        Long to = data.getToId();
        simpMessagingTemplate.convertAndSendToUser(to.toString(), "/" + id + uri, data);
    }
}
