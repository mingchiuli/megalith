package com.chiu.megalith.common.operator;

import lombok.RequiredArgsConstructor;
import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.SessionCallback;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;

/**
 * @author mingchiuli
 * @create 2023-03-05 1:04 am
 */
@Component
@RequiredArgsConstructor
public class CodeOperator {
    private static final char[] cs = {
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
            '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'
    };

    private final StringRedisTemplate redisTemplate;

    public String createCode() {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < 5; i++) {
            int idx = ThreadLocalRandom.current().nextInt(cs.length);
            builder.append(cs[idx]);
        }
        return builder.toString();
    }

    public String saveCode(String code, String prefix) {
        Map<String, Object> map = new HashMap<>(3);
        map.put("code", code);
        map.put("try_count", "0");

        redisTemplate.execute(new SessionCallback<>() {
            @Override
            @SuppressWarnings("unchecked")
            public List<Object> execute(@NonNull RedisOperations operations) throws DataAccessException {
                operations.multi();
                operations.opsForHash().putAll(prefix, map);
                operations.expire(prefix, 120, TimeUnit.SECONDS);
                return operations.exec();
            }
        });
        return code;
    }
}
