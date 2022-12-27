package com.chiu.megalith.websocket.controller;

import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.utils.RedisUtils;
import com.chiu.megalith.websocket.config.CoWorkMQConfig;
import com.chiu.megalith.websocket.dto.impl.ChatInfoDto;
import com.chiu.megalith.websocket.vo.UserEntityVo;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.core.AbstractMessageSendingTemplate;
import org.springframework.messaging.handler.annotation.DestinationVariable;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.simp.SimpMessagingTemplate;
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

    private final RabbitTemplate rabbitTemplate;

    private final StringRedisTemplate redisTemplate;

    private final RedisUtils redisUtils;

    @MessageMapping("/chat")
    public void chat(Principal user, ChatInfoDto.Message msg) {
        msg.setFrom(Long.parseLong(user.getName()));

        msg.getTo().forEach(userId -> {
            String o = (String) redisTemplate.opsForHash().get(Const.COOP_PREFIX.getMsg(), userId);
            UserEntityVo userEntityVo = redisUtils.readValue(o, UserEntityVo.class);

            rabbitTemplate.convertAndSend(
                    CoWorkMQConfig.WS_TOPIC_EXCHANGE, CoWorkMQConfig.WS_BINDING_KEY + userEntityVo.getServerMark(),
                    msg);
        });
    }


}
