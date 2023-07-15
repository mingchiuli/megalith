package org.chiu.megalith.coop.mq.handler;

import org.chiu.megalith.coop.dto.BaseDto;
import org.chiu.megalith.coop.dto.impl.SyncBlogDto;
import org.chiu.megalith.coop.mq.BaseCoopHandler;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
public class SyncBlogHandler extends BaseCoopHandler {

    public SyncBlogHandler(SimpMessagingTemplate simpMessagingTemplate) {
        super(simpMessagingTemplate, "/content");
    }

    @Override
    public boolean supports(BaseDto msg) {
        return msg instanceof SyncBlogDto;
    }
}
