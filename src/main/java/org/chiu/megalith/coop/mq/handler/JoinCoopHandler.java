package org.chiu.megalith.coop.mq.handler;

import org.chiu.megalith.coop.dto.BaseTransferDto;
import org.chiu.megalith.coop.dto.impl.JoinCoopDto;
import org.chiu.megalith.coop.mq.BaseHandler;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

/**
 * @author mingchiuli
 * @create 2023-01-21 4:59 pm
 */
@Component
public class JoinCoopHandler extends BaseHandler {

    public JoinCoopHandler(SimpMessagingTemplate simpMessagingTemplate) {
        super(simpMessagingTemplate, "/join");
    }

    @Override
    public boolean supports(BaseTransferDto msg) {
        return msg instanceof JoinCoopDto;
    }

}
