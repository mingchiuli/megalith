package org.chiu.megalith.coop.mq.handler;

import org.chiu.megalith.coop.dto.BaseTransferDto;
import org.chiu.megalith.coop.dto.impl.FinishCoopDto;
import org.chiu.megalith.coop.mq.BaseHandler;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
public class DestroySessionHandler extends BaseHandler {

    public DestroySessionHandler(SimpMessagingTemplate simpMessagingTemplate) {
        super(simpMessagingTemplate, "/destroy");
    }

    @Override
    public boolean supports(BaseTransferDto msg) {
        return msg instanceof FinishCoopDto;
    }
}
