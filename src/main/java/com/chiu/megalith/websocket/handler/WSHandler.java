package com.chiu.megalith.websocket.handler;

import com.chiu.megalith.websocket.dto.MessageDto;

public interface WSHandler {

    boolean supports(MessageDto msg);

    void handle(MessageDto msg);
}
