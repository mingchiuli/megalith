package com.chiu.megalith.coop.service.impl;

import com.chiu.megalith.exhibit.entity.BlogEntity;
import com.chiu.megalith.exhibit.service.BlogService;
import com.chiu.megalith.exhibit.vo.BlogEntityVo;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.utils.RedisUtils;
import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.service.UserService;
import com.chiu.megalith.coop.config.CoopRabbitConfig;
import com.chiu.megalith.coop.dto.Container;
import com.chiu.megalith.coop.dto.impl.DestroyDto;
import com.chiu.megalith.coop.dto.impl.JoinDto;
import com.chiu.megalith.coop.service.CoopService;
import com.chiu.megalith.coop.vo.InitCoopVo;
import com.chiu.megalith.coop.vo.UserEntityVo;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.concurrent.TimeUnit;

/**
 * @author mingchiuli
 * @create 2022-12-26 1:05 am
 */
@Service
@RequiredArgsConstructor
public class CoopServiceImpl implements CoopService {
    private final UserService userService;

    private final StringRedisTemplate redisTemplate;

    private final RabbitTemplate rabbitTemplate;

    private final BlogService blogService;

    private final RedisUtils redisUtils;

    @Override
    public InitCoopVo joinCoop(Long blogId,
                               Integer orderNumber) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        UserEntity userEntity = userService.findById(userId);
        BlogEntity blogEntity = blogService.findById(blogId);
        UserEntityVo userEntityVo = UserEntityVo.
                builder().
                id(userEntity.getId()).
                avatar(userEntity.getAvatar()).
                username(userEntity.getUsername()).
                build();

        JoinDto.Bind bind = JoinDto.
                Bind.
                builder().
                blogId(blogId).
                fromId(userId).
                user(userEntityVo).
                build();

        JoinDto dto = JoinDto.
                builder().
                data(new Container<>(bind)).
                build();

        Collection<String> usersStr = redisUtils.opsForHashValues(Const.COOP_PREFIX.getInfo() + blogId);

        usersStr.
                stream().
                map(str -> redisUtils.readValue(str, UserEntityVo.class)).
                forEach(user -> {
                    bind.setToOne(user.getId());
                    rabbitTemplate.convertAndSend(
                            CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                            CoopRabbitConfig.WS_BINDING_KEY + user.getServerMark(),
                            dto);
                });

        return InitCoopVo.
                builder().
                blogEntity(blogEntity).
                userEntityVos(usersStr).
                build();
    }

    @Override
    public void submit(Long blogId,
                       BlogEntityVo blogEntityVo) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());
        blogService.saveOrUpdate(blogEntityVo);


        DestroyDto.Bind bind = DestroyDto.
                Bind.
                builder().
                blogId(blogId).
                fromId(userId).
                build();

        DestroyDto dto = DestroyDto.
                builder().
                data(new Container<>(bind)).
                build();

        Collection<String> usersStr = redisUtils.opsForHashValues(Const.COOP_PREFIX.getInfo() + blogId);

        usersStr.
                stream().
                map(str -> redisUtils.readValue(str, UserEntityVo.class)).
                filter(user -> userId != user.getId()).
                forEach(user -> {
                    bind.setToOne(user.getId());
                    rabbitTemplate.convertAndSend(
                            CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                            CoopRabbitConfig.WS_BINDING_KEY + user.getServerMark(),
                            dto);
                });

        redisTemplate.expire(Const.COOP_PREFIX.getInfo() + blogId, 10 , TimeUnit.SECONDS);
    }
}
