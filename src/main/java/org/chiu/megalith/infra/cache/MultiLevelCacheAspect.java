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
import org.redisson.api.*;
import org.springframework.core.annotation.Order;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Objects;

import com.github.benmanes.caffeine.cache.Cache;

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
public class MultiLevelCacheAspect {

    private final StringRedisTemplate redisTemplate;

    private final CacheKeyGenerator cacheKeyGenerator;

    private final ObjectMapper objectMapper;

    private final RedissonClient redisson;

    private final Cache<String, Object> localCache;

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
        var parameterTypes = new Class[pjp.getArgs().length];
        Object[] args = pjp.getArgs();
        for (int i = 0; i < args.length; i++) {
            parameterTypes[i] = args[i].getClass();
        }
        // 参数
        Method method = declaringType.getMethod(methodName, parameterTypes);

        Type genericReturnType = method.getGenericReturnType();
        JavaType javaType;
        
        if (genericReturnType instanceof ParameterizedType parameterizedType) {
            javaType = getTypesReference(parameterizedType);
        } else {
            javaType = objectMapper.getTypeFactory().constructType(genericReturnType);
        }

        String cacheKey = cacheKeyGenerator.generateKey(declaringType, methodName, parameterTypes, args);

        Object cacheValue = localCache.getIfPresent(cacheKey);
        if (Objects.nonNull(cacheValue)) {
            return cacheValue;
        }

        return localCache.get(cacheKey, new CacheTask(redisTemplate, objectMapper, redisson, pjp, javaType, method)::apply);
    }

    private JavaType getTypesReference(ParameterizedType parameterizedType) {
        Class<?> rawType = (Class<?>) parameterizedType.getRawType();
        Type[] arguments = parameterizedType.getActualTypeArguments();
        var javaTypes = new JavaType[arguments.length];
        for (int i = 0; i < javaTypes.length; i++) {
            if (arguments[i] instanceof ParameterizedType parameterizedTyp) {
                JavaType type = getTypesReference(parameterizedTyp);
                javaTypes[i] = type;
            } else {
                javaTypes[i] = objectMapper.getTypeFactory().constructType(arguments[i]);
            }
        }
        return objectMapper.getTypeFactory().constructParametricType(rawType, javaTypes);
    }

}
