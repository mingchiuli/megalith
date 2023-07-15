package org.chiu.megalith.coop.mq.handler;

import org.chiu.megalith.coop.dto.BaseDto;
import org.chiu.megalith.coop.dto.impl.JoinBlogDto;
import org.chiu.megalith.coop.mq.BaseCoopHandler;
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
    public boolean supports(BaseDto msg) {
        return msg instanceof JoinBlogDto;
    }

}
