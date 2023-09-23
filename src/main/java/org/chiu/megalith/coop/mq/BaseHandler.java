package org.chiu.megalith.coop.mq;

import java.util.List;

import org.chiu.megalith.coop.dto.BaseTransferDto;
import org.springframework.messaging.simp.SimpMessagingTemplate;

public abstract class BaseHandler {

    private final SimpMessagingTemplate simpMessagingTemplate;

    protected final String url;

    protected BaseHandler(SimpMessagingTemplate simpMessagingTemplate, String url) {
        this.simpMessagingTemplate = simpMessagingTemplate;
        this.url = url;
    }

    public abstract boolean supports(BaseTransferDto msg);

    public void handle(BaseTransferDto msg) {
        Long blogId = msg.getBlogId();
        List<Long> toIds = msg.getToId();
        toIds.forEach(id -> simpMessagingTemplate.convertAndSendToUser(id.toString(), "/" + blogId + url, msg));
        
    }
}
