package com.chiu.megalith.websocket.mq.handler;

import com.chiu.megalith.websocket.dto.MessageDto;

public interface WSHandler {

    boolean supports(MessageDto msg);

    void handle(MessageDto msg);
}
