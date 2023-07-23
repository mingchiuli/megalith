package org.chiu.megalith.coop.mq.handler;

import org.chiu.megalith.coop.dto.BaseDto;
import org.chiu.megalith.coop.dto.impl.SubmitBlogDto;
import org.chiu.megalith.coop.mq.BaseHandler;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
public class SubmitBlogHandler extends BaseHandler {

    public SubmitBlogHandler(SimpMessagingTemplate simpMessagingTemplate) {
        super(simpMessagingTemplate, "/destroy");
    }

    @Override
    public boolean supports(BaseDto msg) {
        return msg instanceof SubmitBlogDto;
    }
}
