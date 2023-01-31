package com.chiu.megalith.coop.mq.handler;

import com.chiu.megalith.coop.dto.MessageDto;

public interface CoopHandler {

    boolean supports(MessageDto msg);

    void handle(MessageDto msg);
}
