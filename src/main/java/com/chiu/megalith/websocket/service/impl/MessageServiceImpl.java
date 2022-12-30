package com.chiu.megalith.websocket.service.impl;

import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.utils.RedisUtils;
import com.chiu.megalith.websocket.config.CoWorkMQConfig;
import com.chiu.megalith.websocket.dto.impl.ChatInfoDto;
import com.chiu.megalith.websocket.service.MessageService;
import com.chiu.megalith.websocket.vo.UserEntityVo;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.security.Principal;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-12-28 3:43 pm
 */
@Service
@RequiredArgsConstructor
public class MessageServiceImpl implements MessageService {

    private final RabbitTemplate rabbitTemplate;

    private final StringRedisTemplate redisTemplate;

    private final RedisUtils redisUtils;
    @Override
    public void chat(Principal user, ChatInfoDto.Message msg) {
        msg.setFrom(Long.parseLong(user.getName()));

        msg.getToAll().forEach(userId -> {

            String o = (String) redisTemplate.opsForHash().get(Const.COOP_PREFIX.getMsg() + msg.getBlogId(), userId);
            Optional.ofNullable(o).ifPresent(str -> {
                UserEntityVo userEntityVo = redisUtils.readValue(
                        str,
                        UserEntityVo.class);
                msg.setToOne(userId);
                rabbitTemplate.convertAndSend(
                        CoWorkMQConfig.WS_TOPIC_EXCHANGE, CoWorkMQConfig.WS_BINDING_KEY + userEntityVo.getServerMark(),
                        msg);
            });
        });
    }
}
