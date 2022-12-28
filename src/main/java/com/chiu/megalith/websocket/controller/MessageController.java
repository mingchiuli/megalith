package com.chiu.megalith.websocket.controller;

import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.utils.RedisUtils;
import com.chiu.megalith.websocket.config.CoWorkMQConfig;
import com.chiu.megalith.websocket.dto.impl.ChatInfoDto;
import com.chiu.megalith.websocket.service.MessageService;
import com.chiu.megalith.websocket.vo.UserEntityVo;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

import java.security.Principal;

/**
 * @author mingchiuli
 * @create 2022-12-26 4:30 pm
 */
@Controller
@RequiredArgsConstructor
@RequestMapping("/coop")
public class MessageController {

    private final MessageService messageService;


    @MessageMapping("/chat")
    public void chat(Principal user, ChatInfoDto.Message msg) {
        messageService.chat(user, msg);
    }


}
