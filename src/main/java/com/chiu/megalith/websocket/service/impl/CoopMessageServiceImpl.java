package com.chiu.megalith.websocket.service.impl;

import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.utils.RedisUtils;
import com.chiu.megalith.websocket.config.CoopRabbitConfig;
import com.chiu.megalith.websocket.dto.impl.*;
import com.chiu.megalith.websocket.service.CoopMessageService;
import com.chiu.megalith.websocket.vo.UserEntityVo;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-12-28 3:43 pm
 */
@Service
@RequiredArgsConstructor
public class CoopMessageServiceImpl implements CoopMessageService {

    private final RabbitTemplate rabbitTemplate;

    private final StringRedisTemplate redisTemplate;

    private final RedisUtils redisUtils;
    @Override
    public void chat(ChatInfoDto.Message msg) {
        msg.getToAll().forEach(userId -> {
            HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
            String s = hashOperations.get(Const.COOP_PREFIX.getInfo() + msg.getBlogId(), userId);
            Optional.ofNullable(s).ifPresent(str -> {
                UserEntityVo userEntityVo = redisUtils.readValue(
                        str,
                        UserEntityVo.class);
                msg.setToOne(userId);
                rabbitTemplate.convertAndSend(
                        CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                        CoopRabbitConfig.WS_BINDING_KEY + userEntityVo.getServerMark(),
                        msg);
            });
        });
    }

    @Override
    public void sync(SyncContentDto.Content msg) {
        sendToOtherUsers(msg);
    }

    @Override
    public void destroy(DestroyDto.Bind msg) {
        sendToOtherUsers(msg);
    }

    @Override
    public void quit(QuitDto.Bind msg) {
        sendToOtherUsers(msg);
    }

    private void sendToOtherUsers(BaseDto msg) {
        Long from = msg.getFrom();
        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        Map<String, String> users = hashOperations.entries(Const.COOP_PREFIX.getInfo() + msg.getBlogId());
        users.forEach((k, v) -> {
            if (from != Long.parseLong(k)) {
                UserEntityVo userEntityVo = redisUtils.readValue(v, UserEntityVo.class);
                rabbitTemplate.convertAndSend(
                        CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                        CoopRabbitConfig.WS_BINDING_KEY + userEntityVo.getServerMark(),
                        msg);
            }
        });
    }

}
