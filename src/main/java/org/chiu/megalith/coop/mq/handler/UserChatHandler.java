package org.chiu.megalith.coop.mq.handler;

import org.chiu.megalith.coop.dto.BaseDto;
import org.chiu.megalith.coop.dto.impl.UserChatDto;
import org.chiu.megalith.coop.mq.BaseHandler;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
public class UserChatHandler extends BaseHandler {

    public UserChatHandler(SimpMessagingTemplate simpMessagingTemplate) {
        super(simpMessagingTemplate, "/chat");
    }

    @Override
    public boolean supports(BaseDto msg) {
        return msg instanceof UserChatDto;
    }
}
