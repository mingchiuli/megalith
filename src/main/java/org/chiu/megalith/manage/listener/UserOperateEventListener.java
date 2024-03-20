package org.chiu.megalith.manage.listener;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.user.UserIndexMessage;
import org.chiu.megalith.manage.event.UserOperateEvent;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.event.EventListener;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

import static org.chiu.megalith.infra.lang.Const.BLOCK;
import static org.chiu.megalith.infra.lang.Const.BLOCK_USER;


@Component
@RequiredArgsConstructor
public class UserOperateEventListener {

    private final StringRedisTemplate redisTemplate;

    @Value("${blog.jwt.access-token-expire}")
    private long accessExpire;

    @EventListener
    public void process(UserOperateEvent event) {
        UserIndexMessage userIndexMessage = event.getUserIndexMessage();
        String role = userIndexMessage.getRole();
        Long userId = userIndexMessage.getUserId();

        if (BLOCK.getInfo().equals(role)) {
            redisTemplate.opsForValue().set(BLOCK_USER.getInfo() + userId, "", accessExpire, TimeUnit.SECONDS);
        } else {
            redisTemplate.delete(BLOCK_USER.getInfo() + userId);
        }
    }
}
