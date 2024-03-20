package org.chiu.megalith.manage.listener;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.user.UserIndexMessage;
import org.chiu.megalith.manage.event.UserOperateEvent;
import org.springframework.context.event.EventListener;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import static org.chiu.megalith.infra.lang.Const.BLOCK;
import static org.chiu.megalith.infra.lang.Const.BLOCK_USER;


@Component
@RequiredArgsConstructor
public class UserOperateEventListener {

    private final StringRedisTemplate redisTemplate;

    @EventListener
    public void process(UserOperateEvent event) {
        UserIndexMessage userIndexMessage = event.getUserIndexMessage();
        String role = userIndexMessage.getRole();
        Long userId = userIndexMessage.getUserId();

        if (BLOCK.getInfo().equals(role)) {
            redisTemplate.opsForSet().add(BLOCK_USER.getInfo(), userId.toString());
        } else {
            redisTemplate.opsForSet().remove(BLOCK_USER.getInfo(), userId.toString());
        }
    }
}
