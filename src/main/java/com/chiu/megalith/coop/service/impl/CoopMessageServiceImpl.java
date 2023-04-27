package com.chiu.megalith.coop.service.impl;

import com.chiu.megalith.infra.lang.Const;
import com.chiu.megalith.infra.utils.JsonUtils;
import com.chiu.megalith.infra.utils.LuaScriptUtils;
import com.chiu.megalith.coop.config.CoopRabbitConfig;
import com.chiu.megalith.coop.dto.*;
import com.chiu.megalith.coop.service.CoopMessageService;
import com.chiu.megalith.coop.vo.UserEntityVo;
import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.service.UserService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Objects;

/**
 * @author mingchiuli
 * @create 2022-12-28 3:43 pm
 */
@Service
@Slf4j
@RequiredArgsConstructor
public class CoopMessageServiceImpl implements CoopMessageService {

    private final UserService userService;

    private final RabbitTemplate rabbitTemplate;

    private final StringRedisTemplate redisTemplate;

    private final JsonUtils jsonUtils;
    @Override
    public void chatUser(BaseDto msg) {
        sendToOtherUsers(msg);
    }

    @Override
    public void syncBlog(BaseDto msg) {
        sendToOtherUsers(msg);
    }

    @Override
    public void submitBlog(BaseDto msg) {
        sendToOtherUsers(msg);
    }

    @Override
    public void quitBlog(BaseDto msg) {
        sendToOtherUsers(msg);
        redisTemplate.opsForHash().delete(Const.COOP_PREFIX.getInfo() + msg.getBlogId(), msg.getFromId().toString());
    }

    @Override
    public void setUserToRedisSession(Long userId,
                                      Long blogId) {
        UserEntity userEntity = userService.findById(userId);
        UserEntityVo userEntityVo = UserEntityVo.builder()
                .id(userId)
                .avatar(userEntity.getAvatar())
                .nickname(userEntity.getNickname())
                .nodeMark(CoopRabbitConfig.nodeMark)
                .build();

        redisTemplate.execute(LuaScriptUtils.sendUserToSessionLua,
                Collections.singletonList(Const.COOP_PREFIX.getInfo() + blogId),
                userId, jsonUtils.writeValueAsString(userEntityVo), "21600");
    }

    private void sendToOtherUsers(BaseDto msg) {
        Long fromId = msg.getFromId();
        HashOperations<String, String, String> operations = redisTemplate.opsForHash();

        operations.values(Const.COOP_PREFIX.getInfo() + msg.getBlogId()).stream()
                .map(userStr -> jsonUtils.readValue(userStr, UserEntityVo.class))
                .filter(user -> Objects.equals(fromId, user.getId()))
                .forEach(user -> {
                    msg.setToId(user.getId());
                    rabbitTemplate.convertAndSend(
                            CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                            CoopRabbitConfig.WS_BINDING_KEY + user.getNodeMark(),
                            msg);
                });
    }

}
