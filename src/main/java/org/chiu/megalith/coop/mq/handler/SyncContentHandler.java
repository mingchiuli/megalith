package org.chiu.megalith.coop.mq.handler;

import org.chiu.megalith.coop.dto.BaseDto;
import org.chiu.megalith.coop.dto.impl.SyncContentDto;
import org.chiu.megalith.coop.mq.BaseHandler;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
public class SyncContentHandler extends BaseHandler {

    public SyncContentHandler(SimpMessagingTemplate simpMessagingTemplate) {
        super(simpMessagingTemplate, "/content");
    }

    @Override
    public boolean supports(BaseDto msg) {
        return msg instanceof SyncContentDto;
    }
}
