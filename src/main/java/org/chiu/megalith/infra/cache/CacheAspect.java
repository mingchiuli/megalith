package org.chiu.megalith.infra.cache;

import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.Signature;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.chiu.megalith.infra.utils.ClassUtils;
import org.chiu.megalith.infra.utils.JsonUtils;
import org.redisson.api.*;
import org.springframework.core.annotation.Order;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.Objects;

/**
 * 统一缓存处理
 * 
 * @author mingchiuli
 *         order: 多个切面执行顺序，越小越先执行
 * @create 2021-12-01 7:48 AM
 */
@Aspect
@Component
@Order(2)
@RequiredArgsConstructor
public class CacheAspect {

    private final StringRedisTemplate redisTemplate;

    private final CacheKeyGenerator cacheKeyGenerator;

    private final ObjectMapper objectMapper;

    private final RedissonClient redisson;

    private final JsonUtils jsonUtils;

    private final com.github.benmanes.caffeine.cache.Cache<String, Object> localCache;

    @Pointcut("@annotation(org.chiu.megalith.infra.cache.Cache)")
    public void pt() {
    }

    @SneakyThrows
    @Around("pt()")
    public Object around(ProceedingJoinPoint pjp) {
        Signature signature = pjp.getSignature();
        // 类名
        // 调用的方法名
        String methodName = signature.getName();
        Class<?> declaringType = signature.getDeclaringType();
        Object[] args = pjp.getArgs();
        Class<?>[] parameterTypes = ClassUtils.findClassArray(args);

        // 参数
        Method method = declaringType.getMethod(methodName, parameterTypes);

        JavaType javaType = jsonUtils.getTypesReference(method);
        String cacheKey = cacheKeyGenerator.generateKey(method, args);

        Object cacheValue = localCache.getIfPresent(cacheKey);
        if (Objects.nonNull(cacheValue)) {
            return cacheValue;
        }

        return localCache.get(cacheKey, new CacheTask(redisTemplate, objectMapper, redisson, pjp, javaType, method));
    }

}
