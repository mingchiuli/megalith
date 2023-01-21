package com.chiu.megalith.ws.mq.handler;

import com.chiu.megalith.ws.dto.MessageDto;

public interface CoopHandler {

    boolean supports(MessageDto msg);

    void handle(MessageDto msg);
}
