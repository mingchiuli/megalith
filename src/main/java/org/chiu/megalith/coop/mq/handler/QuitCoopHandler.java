package org.chiu.megalith.coop.mq.handler;

import org.chiu.megalith.coop.dto.BaseDto;
import org.chiu.megalith.coop.dto.impl.QuitBlogDto;
import org.chiu.megalith.coop.mq.BaseHandler;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

/**
 * @author mingchiuli
 * @create 2022-12-30 11:55 pm
 */
@Component
public class QuitCoopHandler extends BaseHandler {

    public QuitCoopHandler(SimpMessagingTemplate simpMessagingTemplate) {
        super(simpMessagingTemplate, "/quit");
    }

    @Override
    public boolean supports(BaseDto msg) {
        return msg instanceof QuitBlogDto;
    }
}
