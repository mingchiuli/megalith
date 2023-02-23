package com.chiu.megalith.common.utils;

import com.chiu.megalith.common.lang.Const;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.springframework.amqp.rabbit.connection.CorrelationData;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.TimeUnit;


@Component
@RequiredArgsConstructor
public class RedisJsonUtils {

    private final StringRedisTemplate redisTemplate;

    private final ObjectMapper objectMapper;


    @SneakyThrows
    public <T> T readValue(String str, Class<T> clazz) {
        return objectMapper.readValue(str, clazz);
    }

    @SneakyThrows
    public String writeValueAsString(Object obj) {
        return objectMapper.writeValueAsString(obj);
    }

    public Collection<String> opsForHashValues(String key) {
        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        return hashOperations.values(key);
    }

    public Collection<String> opsForHashValues(String key, String exceptKey) {
        HashOperations<String, String, String> hashOperations = redisTemplate.opsForHash();
        Map<String, String> entries = hashOperations.entries(key);
        entries.remove(exceptKey);
        return entries.values();
    }

    public CorrelationData setBlogRedisKeyForEsProcess(String type, Long blogId) {
        CorrelationData correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                type + "_" + blogId,
                30,
                TimeUnit.MINUTES);
        return correlationData;
    }
}
