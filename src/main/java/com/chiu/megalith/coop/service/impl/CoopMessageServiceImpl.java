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
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.util.Collection;
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
        sendToOtherUsers(msg);
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
        redisTemplate.opsForHash().delete(Const.COOP_PREFIX.getInfo() + msg.getBlogId(), msg.getFromId().toString());
    }

    @Override
    public void setServerMark(Long userId, Long blogId) {
        UserEntityVo userEntityVo = redisUtils.opsForHashToObj(Const.COOP_PREFIX.getInfo() + blogId, userId, UserEntityVo.class);
        userEntityVo.setServerMark(CoopRabbitConfig.serverMark);

        redisTemplate.opsForHash().put(Const.COOP_PREFIX.getInfo() + blogId, userId, userEntityVo);
        redisTemplate.expire(Const.COOP_PREFIX.getInfo() + blogId, 6, TimeUnit.HOURS);
    }

    private void sendToOtherUsers(BaseBind msg) {
        Long fromId = msg.getFromId();
        Collection<String> users = redisUtils.opsForHashValues(Const.COOP_PREFIX.getInfo() + msg.getBlogId(), msg.getFromId().toString());

        users.
                stream().
                map(userStr -> redisUtils.readValue(userStr, UserEntityVo.class)).
                filter(user -> !fromId.equals(user.getId())).
                peek(user -> msg.setToOne(user.getId())).
                map(UserEntityVo::getServerMark).
                distinct().
                forEach(serverMark -> rabbitTemplate.convertAndSend(
                        CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                        CoopRabbitConfig.WS_BINDING_KEY + serverMark,
                        msg));
    }

}
