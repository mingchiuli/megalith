package org.chiu.megalith.coop.mq.handler;

import org.chiu.megalith.coop.dto.BaseTransferDto;
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
    public boolean supports(BaseTransferDto msg) {
        return msg instanceof SyncContentDto;
    }
}
