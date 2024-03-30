package org.chiu.megalith.infra.cache;

import java.lang.reflect.Method;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;

import org.aspectj.lang.ProceedingJoinPoint;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.core.NestedRuntimeException;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.StringUtils;

import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.AllArgsConstructor;
import lombok.SneakyThrows;

import static org.chiu.megalith.infra.lang.ExceptionMessage.GET_LOCK_TIMEOUT;

@AllArgsConstructor
public class CacheTask implements Function<String, Object> {

    private static final String LOCK = "blogLock:";

    private StringRedisTemplate redisTemplate;

    private ObjectMapper objectMapper;

    private RedissonClient redisson;

    private ProceedingJoinPoint pjp;

    private JavaType javaType;

    private Method method;

    @Override
    @SneakyThrows
    public Object apply(String key) {

        String o;
        // 防止redis挂了以后db也访问不了
        try {
            o = redisTemplate.opsForValue().get(key);
        } catch (NestedRuntimeException e) {
            return pjp.proceed();
        }

        if (StringUtils.hasLength(o)) {
            return objectMapper.readValue(o, javaType);
        }

        String lock = LOCK + key;
        // 已经线程安全
        RLock rLock = redisson.getLock(lock);

        if (Boolean.FALSE.equals(rLock.tryLock(5000, TimeUnit.MILLISECONDS))) {
            throw new TimeoutException(GET_LOCK_TIMEOUT.getMsg());
        }

        try {
            // 双重检查
            String r = redisTemplate.opsForValue().get(key);

            if (StringUtils.hasLength(r)) {
                return objectMapper.readValue(r, javaType);
            }
            // 执行目标方法
            Object proceed = pjp.proceed();

            Cache annotation = method.getAnnotation(Cache.class);
            redisTemplate.opsForValue().set(key, objectMapper.writeValueAsString(proceed), annotation.expire(), TimeUnit.MINUTES);
            return proceed;
        } finally {
            rLock.unlock();
        }
    }

}
