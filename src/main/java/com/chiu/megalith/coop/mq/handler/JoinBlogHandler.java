package com.chiu.megalith.coop.mq.handler;

import com.chiu.megalith.coop.dto.MessageDto;
import com.chiu.megalith.coop.dto.impl.JoinBlogDto;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

/**
 * @author mingchiuli
 * @create 2023-01-21 4:59 pm
 */
@Component
public class JoinBlogHandler extends BaseCoopHandler {

    public JoinBlogHandler(SimpMessagingTemplate simpMessagingTemplate) {
        super(simpMessagingTemplate, "/join");
    }

    @Override
    public boolean supports(MessageDto msg) {
        return msg instanceof JoinBlogDto;
    }

}
