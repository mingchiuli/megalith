package com.chiu.megalith.coop.mq.handler;

import com.chiu.megalith.coop.dto.BaseDto;
import com.chiu.megalith.coop.dto.impl.SubmitBlogDto;
import com.chiu.megalith.coop.mq.BaseCoopHandler;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
public class SubmitBlogHandler extends BaseCoopHandler {

    public SubmitBlogHandler(SimpMessagingTemplate simpMessagingTemplate) {
        super(simpMessagingTemplate, "/destroy");
    }

    @Override
    public boolean supports(BaseDto msg) {
        return msg instanceof SubmitBlogDto;
    }
}
