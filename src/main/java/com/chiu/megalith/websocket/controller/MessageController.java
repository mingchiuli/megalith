package com.chiu.megalith.websocket.controller;

import org.springframework.messaging.handler.annotation.DestinationVariable;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.stereotype.Controller;

import java.security.Principal;

/**
 * @author mingchiuli
 * @create 2022-12-26 4:30 pm
 */
@Controller
public class MessageController {

    @MessageMapping("/chat/{to}/{blogId}")
    public void chat(Principal user, String msg, @DestinationVariable Long to, @DestinationVariable Long blogId) {
        System.out.println(msg);
    }
}
