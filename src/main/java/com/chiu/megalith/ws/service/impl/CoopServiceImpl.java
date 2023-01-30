package com.chiu.megalith.ws.service.impl;

import com.chiu.megalith.exhibit.entity.BlogEntity;
import com.chiu.megalith.exhibit.service.BlogService;
import com.chiu.megalith.exhibit.vo.BlogEntityVo;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.utils.RedisUtils;
import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.service.UserService;
import com.chiu.megalith.ws.config.CoopRabbitConfig;
import com.chiu.megalith.ws.dto.Container;
import com.chiu.megalith.ws.dto.impl.DestroyDto;
import com.chiu.megalith.ws.dto.impl.JoinDto;
import com.chiu.megalith.ws.service.CoopService;
import com.chiu.megalith.ws.vo.InitCoopVo;
import com.chiu.megalith.ws.vo.UserEntityVo;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.SessionCallback;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.lang.NonNull;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;
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
    public InitCoopVo joinCoop(Long blogId, Integer orderNumber) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        UserEntity userEntity = userService.findById(userId);
        BlogEntity blogEntity = blogService.findById(blogId);
        UserEntityVo userEntityVo = UserEntityVo.
                builder().
                id(userEntity.getId()).
                avatar(userEntity.getAvatar()).
                username(userEntity.getUsername()).
                orderNumber(orderNumber).
                serverMark(CoopRabbitConfig.serverMark).
                build();

        redisTemplate.execute(new SessionCallback<>() {
            @Override
            @SuppressWarnings("unchecked")
            public List<Object> execute(@NonNull RedisOperations operations) throws DataAccessException {
                operations.multi();
                operations.opsForHash().put(Const.COOP_PREFIX.getInfo() + blogId,
                        String.valueOf(userId),
                        redisUtils.writeValueAsString(userEntityVo));
                operations.expire(Const.COOP_PREFIX.getInfo() + blogId, 6, TimeUnit.HOURS);
                return operations.exec();
            }
        });

        userEntityVo.setServerMark(null);

        JoinDto dto = JoinDto.builder().
                data(new Container<>(
                        JoinDto.Bind.
                                builder().
                                blogId(blogId).
                                from(userId).
                                user(userEntityVo).
                                build()
                )).
                build();

        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        List<String> usersStr = hashOperations.values(Const.COOP_PREFIX.getInfo() + blogId);

        List<UserEntityVo> userEntityInfos = usersStr.
                stream().
                map(str -> redisUtils.readValue(str, UserEntityVo.class)).
                filter(user -> user.getId() != userId).
                sorted(Comparator.comparing(UserEntityVo::getOrderNumber)).
                toList();

        userEntityInfos.
                stream().
                map(UserEntityVo::getServerMark).
                distinct().
                forEach(serverMark ->
                        rabbitTemplate.convertAndSend(
                                CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                                CoopRabbitConfig.WS_BINDING_KEY + serverMark,
                                dto));

        userEntityInfos.forEach(user -> user.setServerMark(null));

        return InitCoopVo.
                builder().
                blogEntity(blogEntity).
                userEntityVos(userEntityInfos).
                build();
    }

    @Override
    public void submit(Long blogId, BlogEntityVo blogEntityVo) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());
        blogService.saveOrUpdate(blogEntityVo);

        DestroyDto dto = DestroyDto.
                builder().
                data(new Container<>(
                        DestroyDto.Bind.
                                builder().
                                blogId(blogId).
                                from(userId).
                                build())
                ).
                build();

        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        List<String> usersStr = hashOperations.values(Const.COOP_PREFIX.getInfo() + blogId);

        usersStr.
                stream().
                map(str -> redisUtils.readValue(str, UserEntityVo.class)).
                filter(user -> userId != user.getId()).
                map(UserEntityVo::getServerMark).
                distinct().
                forEach(serverMark -> rabbitTemplate.convertAndSend(
                        CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                        CoopRabbitConfig.WS_BINDING_KEY + serverMark,
                        dto));

        redisTemplate.expire(Const.COOP_PREFIX.getInfo() + blogId, 10 , TimeUnit.SECONDS);
    }
}
