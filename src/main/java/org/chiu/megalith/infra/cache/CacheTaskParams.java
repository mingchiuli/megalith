package org.chiu.megalith.infra.cache;

import java.lang.reflect.Method;

import org.aspectj.lang.ProceedingJoinPoint;
import org.redisson.api.RedissonClient;
import org.springframework.data.redis.core.StringRedisTemplate;

import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class CacheTaskParams {

    private StringRedisTemplate redisTemplate;

    private ObjectMapper objectMapper;

    private RedissonClient redisson;

    private ProceedingJoinPoint pjp;

    private JavaType javaType;

    private Method method;
}
