package org.chiu.megalith.coop.mq;

import org.chiu.megalith.coop.dto.BaseDto;
import org.springframework.messaging.simp.SimpMessagingTemplate;

public abstract class BaseHandler {

    private final SimpMessagingTemplate simpMessagingTemplate;

    protected final String uri;

    protected BaseHandler(SimpMessagingTemplate simpMessagingTemplate,
                              String uri) {
        this.simpMessagingTemplate = simpMessagingTemplate;
        this.uri = uri;
    }

    public abstract boolean supports(BaseDto msg);

    public void handle(BaseDto msg) {
        Long blogId = msg.getBlogId();
        Long to = msg.getToId();
        simpMessagingTemplate.convertAndSendToUser(to.toString(), "/" + blogId + uri, msg);
    }
}
