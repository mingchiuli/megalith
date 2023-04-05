package com.chiu.megalith.coop.mq.handler;

import com.chiu.megalith.coop.dto.MessageDto;
import com.chiu.megalith.coop.dto.impl.QuitBlogDto;
import com.chiu.megalith.coop.mq.BaseCoopHandler;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

/**
 * @author mingchiuli
 * @create 2022-12-30 11:55 pm
 */
@Component
public class QuitBlogHandler extends BaseCoopHandler {

    public QuitBlogHandler(SimpMessagingTemplate simpMessagingTemplate) {
        super(simpMessagingTemplate, "/quit");
    }

    @Override
    public boolean supports(MessageDto msg) {
        return msg instanceof QuitBlogDto;
    }

}
