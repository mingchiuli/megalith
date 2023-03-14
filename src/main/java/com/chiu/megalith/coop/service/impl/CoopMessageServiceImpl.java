package com.chiu.megalith.coop.service.impl;

import com.chiu.megalith.base.lang.Const;
import com.chiu.megalith.base.utils.JsonUtils;
import com.chiu.megalith.coop.config.CoopRabbitConfig;
import com.chiu.megalith.coop.dto.*;
import com.chiu.megalith.coop.service.CoopMessageService;
import com.chiu.megalith.coop.vo.UserEntityVo;
import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.service.UserService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.SessionCallback;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

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
    public void chat(MessageDto.BaseBind msg) {
        sendToOtherUsers(msg);
    }

    @Override
    public void syncContent(MessageDto.BaseBind msg) {
        sendToOtherUsers(msg);
    }

    @Override
    public void destroy(MessageDto.BaseBind msg) {
        sendToOtherUsers(msg);
    }

    @Override
    public void quit(MessageDto.BaseBind msg) {
        sendToOtherUsers(msg);
        redisTemplate.opsForHash().delete(Const.COOP_PREFIX.getInfo() + msg.getBlogId(), msg.getFromId().toString());
    }

    @Override
    public void setUserToRedisSession(Long userId, Long blogId) {
        UserEntity userEntity = userService.findById(userId);
        UserEntityVo userEntityVo = UserEntityVo.
                builder().
                id(userEntity.getId()).
                avatar(userEntity.getAvatar()).
                username(userEntity.getUsername()).
                nodeMark(CoopRabbitConfig.nodeMark).
                build();

        redisTemplate.execute(new SessionCallback<>() {
            @Override
            @SuppressWarnings("unchecked")
            public List<Object> execute(@NonNull RedisOperations operations) throws DataAccessException {
                operations.multi();
                operations.opsForHash().put(Const.COOP_PREFIX.getInfo() + blogId, userId, userEntityVo);
                operations.expire(Const.COOP_PREFIX.getInfo() + blogId, 6, TimeUnit.HOURS);
                return operations.exec();
            }
        });
    }

    private void sendToOtherUsers(MessageDto.BaseBind msg) {
        Long fromId = msg.getFromId();
        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        Map<String, String> entries = hashOperations.entries(Const.COOP_PREFIX.getInfo() + msg.getBlogId());
        entries.remove(msg.getFromId().toString());

        entries.values().
                stream().
                map(userStr -> jsonUtils.readValue(userStr, UserEntityVo.class)).
                filter(user -> !fromId.equals(user.getId())).
                forEach(user -> {
                    msg.setToOne(user.getId());
                    rabbitTemplate.convertAndSend(
                            CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                            CoopRabbitConfig.WS_BINDING_KEY + user.getNodeMark(),
                            msg);
                });
    }

}
