package com.chiu.megalith.common.utils;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.concurrent.TimeUnit;


@Component
@RequiredArgsConstructor
public class RedisUtils {

    private final StringRedisTemplate redisTemplate;

    private final ObjectMapper objectMapper;

    public boolean tryLock(String key, String val, long timeOut, long expireTime){
        long start = System.currentTimeMillis();
        for (;;) {
            Boolean b = redisTemplate.opsForValue().setIfAbsent(key, val, expireTime, TimeUnit.MILLISECONDS);
            if (Boolean.TRUE.equals(b)){
                return true;
            }
            if (System.currentTimeMillis() - start > timeOut){
                return false;
            }
        }
    }

    public void unLock(String key, String val){
        String script = "if redis.call('get', KEYS[1]) == ARGV[1] then return redis.call('del', KEYS[1]) else return 0 end";
        RedisScript<Long> redisScript = new DefaultRedisScript<>(script, Long.class);
        redisTemplate.execute(redisScript, Collections.singletonList(key), val);
    }

    public Long delete(String key, String val){
        String script = "if redis.call('get', KEYS[1]) == ARGV[1] then return redis.call('del', KEYS[1]) else return 0 end";
        RedisScript<Long> redisScript = new DefaultRedisScript<>(script, Long.class);
        return redisTemplate.execute(redisScript, Collections.singletonList(key), val);
    }

    @SneakyThrows
    public <T> T readValue(String str, Class<T> clazz) {
        return objectMapper.readValue(str, clazz);
    }

    @SneakyThrows
    public String writeValueAsString(Object obj) {
        return objectMapper.writeValueAsString(obj);
    }
}
