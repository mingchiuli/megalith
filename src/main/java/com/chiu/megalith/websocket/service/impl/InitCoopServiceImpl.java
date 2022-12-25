package com.chiu.megalith.websocket.service.impl;

import com.chiu.megalith.blog.entity.BlogEntity;
import com.chiu.megalith.blog.service.BlogService;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.utils.RedisUtils;
import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.service.UserService;
import com.chiu.megalith.websocket.config.CoWorkMQConfig;
import com.chiu.megalith.websocket.dto.Container;
import com.chiu.megalith.websocket.dto.impl.InitDto;
import com.chiu.megalith.websocket.service.InitCoopService;
import com.chiu.megalith.websocket.vo.UserEntityVo;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * @author mingchiuli
 * @create 2022-12-26 1:05 am
 */
@Service
@RequiredArgsConstructor
public class InitCoopServiceImpl implements InitCoopService {
    private final UserService userService;

    private final StringRedisTemplate redisTemplate;

    private final RabbitTemplate rabbitTemplate;

    private final BlogService blogService;

    private final RedisUtils redisUtils;

    @Override
    public Map<String, Object> initCoop(Long blogId, Integer orderNumber) {
        Long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        UserEntity userEntity = userService.findById(userId);
        BlogEntity blogEntity = blogService.findById(blogId);
        UserEntityVo vo = UserEntityVo.builder().
                id(userEntity.getId()).
                avatar(userEntity.getAvatar()).
                username(userEntity.getUsername()).
                orderNumber(orderNumber).
                serverMark(CoWorkMQConfig.serverMark).
                build();

        redisTemplate.opsForHash().putIfAbsent(Const.COOP_PREFIX.getMsg() + blogId, userId.toString(), vo);
        redisTemplate.expire(Const.COOP_PREFIX.getMsg() + blogId, 6 * 60, TimeUnit.MINUTES);

        Map<Object, Object> userMap = redisTemplate.opsForHash().entries(Const.COOP_PREFIX.getMsg() + blogId);

        List<UserEntityVo> userEntityInfos = userMap.values().
                stream().
                map(str -> redisUtils.readValue((String) str, UserEntityVo.class)).
                sorted(Comparator.comparing(UserEntityVo::getOrderNumber)).
                toList();

        List<UserEntityVo> userEntityVos = userEntityInfos.
                stream().
                map(userEntityInfo -> UserEntityVo.builder().
                        id(userEntityInfo.getId()).
                        username(userEntityInfo.getUsername()).
                        avatar(userEntityInfo.getAvatar()).
                        orderNumber(userEntityInfo.getOrderNumber()).
                        build()).
                toList();


        InitDto dto = InitDto.builder().
                data(
                        new Container<>(
                                InitDto.Bind.builder().
                                        blogId(blogId).
                                        users(userEntityVos).
                                        build())
                ).
                build();


        userEntityInfos.forEach(user -> {
            if (!user.getServerMark().equals(CoWorkMQConfig.serverMark)) {
                rabbitTemplate.convertAndSend(
                        CoWorkMQConfig.WS_TOPIC_EXCHANGE,
                        CoWorkMQConfig.WS_BINDING_KEY + user.getServerMark(),
                        dto);
            }
        });

        Map<String, Object> map = new HashMap<>();
        map.put("blog", blogEntity);
        map.put("users", userEntityVos);
        return map;
    }
}
