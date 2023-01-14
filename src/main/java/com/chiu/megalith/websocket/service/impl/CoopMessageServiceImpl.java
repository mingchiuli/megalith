package com.chiu.megalith.websocket.service.impl;

import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.utils.RedisUtils;
import com.chiu.megalith.websocket.config.CoopConfig;
import com.chiu.megalith.websocket.dto.impl.ChatInfoDto;
import com.chiu.megalith.websocket.dto.impl.DestroyDto;
import com.chiu.megalith.websocket.dto.impl.QuitDto;
import com.chiu.megalith.websocket.dto.impl.SyncContentDto;
import com.chiu.megalith.websocket.service.CoopMessageService;
import com.chiu.megalith.websocket.vo.UserEntityVo;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.security.Principal;
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
    public void chat(Principal user, ChatInfoDto.Message msg) {

        msg.getToAll().forEach(userId -> {

            HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
            String o = hashOperations.get(Const.COOP_PREFIX.getMsg() + msg.getBlogId(), userId);
            Optional.ofNullable(o).ifPresent(str -> {
                UserEntityVo userEntityVo = redisUtils.readValue(
                        str,
                        UserEntityVo.class);
                msg.setToOne(userId);
                rabbitTemplate.convertAndSend(
                        CoopConfig.WS_TOPIC_EXCHANGE,
                        CoopConfig.WS_BINDING_KEY + userEntityVo.getServerMark(),
                        msg);
            });
        });
    }

    @Override
    public void sync(Principal user, SyncContentDto.Content msg) {
        Long from = Long.parseLong(user.getName());
        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        Map<String, String> entries = hashOperations.entries(Const.COOP_PREFIX.getMsg() + msg.getBlogId());

        entries.forEach((k, v) -> {
            if (!from.equals(Long.parseLong(k))) {
                UserEntityVo userEntityVo = redisUtils.readValue(v, UserEntityVo.class);
                rabbitTemplate.convertAndSend(
                        CoopConfig.WS_TOPIC_EXCHANGE,
                        CoopConfig.WS_BINDING_KEY + userEntityVo.getServerMark(),
                        msg);
            }
        });
    }

    @Override
    public void destroy(Principal user, DestroyDto.Bind msg) {
        Long from = Long.parseLong(user.getName());
        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        Map<String, String> entries = hashOperations.entries(Const.COOP_PREFIX.getMsg() + msg.getBlogId());
        entries.forEach((k, v) -> {
            if (!from.equals(Long.parseLong(k))) {
                UserEntityVo userEntityVo = redisUtils.readValue(v, UserEntityVo.class);
                rabbitTemplate.convertAndSend(
                        CoopConfig.WS_TOPIC_EXCHANGE,
                        CoopConfig.WS_BINDING_KEY + userEntityVo.getServerMark(),
                        msg);
            }
        });
    }

    @Override
    public void quit(Principal user, QuitDto.Bind msg) {
        Long from = Long.parseLong(user.getName());
        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        Map<String, String> entries = hashOperations.entries(Const.COOP_PREFIX.getMsg() + msg.getBlogId());
        entries.forEach((k, v) -> {
            if (!from.equals(Long.parseLong(k))) {
                UserEntityVo userEntityVo = redisUtils.readValue(v, UserEntityVo.class);
                rabbitTemplate.convertAndSend(
                        CoopConfig.WS_TOPIC_EXCHANGE,
                        CoopConfig.WS_BINDING_KEY + userEntityVo.getServerMark(),
                        msg);
            }
        });
    }
}