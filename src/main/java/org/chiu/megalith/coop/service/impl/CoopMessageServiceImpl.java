package org.chiu.megalith.coop.service.impl;

import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.utils.JsonUtils;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.chiu.megalith.coop.config.CoopRabbitConfig;
import org.chiu.megalith.coop.dto.*;
import org.chiu.megalith.coop.service.CoopMessageService;
import org.chiu.megalith.coop.vo.UserEntityVo;
import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.service.UserService;
import lombok.RequiredArgsConstructor;
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
    public void setUserToRedisSession(Long userId, Long blogId) {
        UserEntity userEntity = userService.findById(userId);
        var userEntityVo = UserEntityVo.builder()
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
                .filter(user -> Boolean.FALSE.equals(Objects.equals(fromId, user.getId())))
                .forEach(user -> {
                    msg.setToId(user.getId());
                    rabbitTemplate.convertAndSend(
                            CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                            CoopRabbitConfig.WS_BINDING_KEY + user.getNodeMark(),
                            msg);
                });
    }

}
