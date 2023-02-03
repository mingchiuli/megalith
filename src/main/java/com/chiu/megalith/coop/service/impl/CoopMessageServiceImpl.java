package com.chiu.megalith.coop.service.impl;

import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.utils.RedisUtils;
import com.chiu.megalith.coop.config.CoopRabbitConfig;
import com.chiu.megalith.coop.dto.BaseBind;
import com.chiu.megalith.coop.dto.impl.*;
import com.chiu.megalith.coop.service.CoopMessageService;
import com.chiu.megalith.coop.vo.UserEntityVo;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

/**
 * @author mingchiuli
 * @create 2022-12-28 3:43 pm
 */
@Service
@Slf4j
@RequiredArgsConstructor
public class CoopMessageServiceImpl implements CoopMessageService {

    private final RabbitTemplate rabbitTemplate;

    private final StringRedisTemplate redisTemplate;

    private final RedisUtils redisUtils;
    @Override
    public void chat(ChatDto.Bind msg) {
        msg.getToAll().forEach(userId -> {

            HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
            String obj = hashOperations.get(Const.COOP_PREFIX.getInfo() + msg.getBlogId(), userId);

            Optional.ofNullable(obj).ifPresentOrElse(userStr -> {
                UserEntityVo userEntityVo = redisUtils.readValue(userStr, UserEntityVo.class);
                msg.setToOne(userId);
                rabbitTemplate.convertAndSend(
                        CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                        CoopRabbitConfig.WS_BINDING_KEY + userEntityVo.getServerMark(),
                        msg);
            }, () ->
                    log.error("user's session error"));
        });
    }

    @Override
    public void syncContent(SyncContentDto.Bind msg) {
        sendToOtherUsers(msg);
    }

    @Override
    public void destroy(DestroyDto.Bind msg) {
        sendToOtherUsers(msg);
    }

    @Override
    public void quit(QuitDto.Bind msg) {
        sendToOtherUsers(msg);
        redisTemplate.opsForHash().delete(Const.COOP_PREFIX.getInfo() + msg.getBlogId(), msg.getFrom());
    }

    @Override
    public void setServerMark(Long userId, Long blogId) {
        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        String userStr = hashOperations.get(Const.COOP_PREFIX.getInfo() + blogId, blogId);
        UserEntityVo userEntityVo = redisUtils.readValue(userStr, UserEntityVo.class);
        userEntityVo.setServerMark(CoopRabbitConfig.serverMark);

        redisTemplate.opsForHash().put(Const.COOP_PREFIX.getInfo() + blogId, userId, userEntityVo);
        redisTemplate.expire(Const.COOP_PREFIX.getInfo() + blogId, 6, TimeUnit.HOURS);
    }

    private void sendToOtherUsers(BaseBind msg) {
        Long from = msg.getFrom();
        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        Map<String, String> users = hashOperations.entries(Const.COOP_PREFIX.getInfo() + msg.getBlogId());

        users.values().
                stream().
                map(userStr -> redisUtils.readValue(userStr, UserEntityVo.class)).
                filter(user -> !from.equals(user.getId())).
                map(UserEntityVo::getServerMark).
                distinct().
                forEach(serverMark -> rabbitTemplate.convertAndSend(
                        CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                        CoopRabbitConfig.WS_BINDING_KEY + serverMark,
                        msg));
    }

}
