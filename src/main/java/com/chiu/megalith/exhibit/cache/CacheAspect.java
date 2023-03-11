package com.chiu.megalith.exhibit.cache;

import com.chiu.megalith.common.utils.JsonUtils;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.Signature;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.redisson.api.*;
import org.springframework.core.NestedRuntimeException;
import org.springframework.core.annotation.Order;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.time.Duration;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * 统一缓存处理
 * @author mingchiuli
 * order: 多个切面执行顺序，越小越先执行
 * @create 2021-12-01 7:48 AM
 */
@Aspect
@Component
@Slf4j
@Order(2)
@RequiredArgsConstructor
public class CacheAspect {

    private static final String LOCK = "blogLock:";

    private final StringRedisTemplate redisTemplate;

    private final JsonUtils jsonUtils;

    private final ObjectMapper objectMapper;

    private final RedissonClient redisson;

    private final LoadingCache<String, RLock> lockCache = Caffeine.
            newBuilder().
            maximumSize(500).
            expireAfterWrite(Duration.ofMinutes(60)).
            build(this::createRlock);


    @Pointcut("@annotation(com.chiu.megalith.exhibit.cache.Cache)")
    public void pt() {}

    @SneakyThrows
    @Around("pt()")
    public Object around(ProceedingJoinPoint pjp) {
        Signature signature = pjp.getSignature();
        //类名
        String className = pjp.getTarget().getClass().getSimpleName();
        //调用的方法名
        String methodName = signature.getName();

        Class<?>[] parameterTypes = new Class[pjp.getArgs().length];
        Object[] args = pjp.getArgs();
        //参数
        StringBuilder params = new StringBuilder();

        for (int i = 0; i < args.length; i++) {
            params.append("::");
            if (args[i] instanceof String) {
                params.append(args[i]);
            } else {
                params.append(jsonUtils.writeValueAsString(args[i]));
            }
            parameterTypes[i] = args[i].getClass();
        }

        Class<?> declaringType = signature.getDeclaringType();
        Method method = declaringType.getMethod(methodName, parameterTypes);

        Cache annotation = method.getAnnotation(Cache.class);
        int expire = ThreadLocalRandom.current().nextInt(annotation.expire()) + 1;
        String prefix = annotation.prefix().getInfo();

        Type genericReturnType = method.getGenericReturnType();

        JavaType javaType;

        if (genericReturnType instanceof ParameterizedType parameterizedType) {
            javaType = getTypesReference(parameterizedType);
        } else {
            javaType = objectMapper.getTypeFactory().constructType(genericReturnType);
        }

        String redisKey = StringUtils.hasLength(prefix) ? prefix + "::" + className + "::" + methodName + params : className + "::" + methodName + params;

        String o;

        //防止redis挂了以后db也访问不了
        try {
            o = redisTemplate.opsForValue().get(redisKey);
        } catch (NestedRuntimeException e) {
            return pjp.proceed();
        }

        if (StringUtils.hasLength(o)) {
            return objectMapper.readValue(o, javaType);
        }

        String lock = LOCK + className + methodName + params;

        //已经线程安全
        RLock rLock = lockCache.get(lock);

        if (!rLock.tryLock(5000, TimeUnit.MILLISECONDS)) {
            throw new TimeoutException("request timeout");
        }

        try {
            //双重检查
            String r = redisTemplate.opsForValue().get(redisKey);

            if (StringUtils.hasLength(r)) {
                return objectMapper.readValue(r, javaType);
            }
            //执行目标方法
            Object proceed = pjp.proceed();
            redisTemplate.opsForValue().set(redisKey, objectMapper.writeValueAsString(proceed), expire, TimeUnit.MINUTES);
            return proceed;
        } finally {
            rLock.unlock();
        }
    }

    private JavaType getTypesReference(ParameterizedType parameterizedType) {
        Class<?> rawType = (Class<?>) parameterizedType.getRawType();
        Type[] arguments = parameterizedType.getActualTypeArguments();
        JavaType[] javaTypes = new JavaType[arguments.length];
        for (int i = 0; i < javaTypes.length; i++) {
            if (arguments[i] instanceof ParameterizedType _parameterizedType) {
                JavaType type = getTypesReference(_parameterizedType);
                javaTypes[i] = type;
            } else {
                javaTypes[i] = objectMapper.getTypeFactory().constructType(arguments[i]);
            }
        }
        return objectMapper.getTypeFactory().constructParametricType(rawType, javaTypes);
    }

    private RLock createRlock(String key) {
        return redisson.getLock(key);
    }
}
