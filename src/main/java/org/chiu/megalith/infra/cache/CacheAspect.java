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
import org.chiu.megalith.infra.config.CacheRabbitConfig;
import org.redisson.api.*;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.core.NestedRuntimeException;
import org.springframework.core.annotation.Order;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Objects;
import java.util.Random;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.springframework.util.StringUtils;

import static org.chiu.megalith.infra.lang.ExceptionMessage.GET_LOCK_TIMEOUT;

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

    private final RabbitTemplate rabbitTemplate;

    private final com.github.benmanes.caffeine.cache.Cache<String, String> localCache;

    private static final String LOCK = "blogLock:";


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

        String cacheValue = localCache.getIfPresent(cacheKey);
        if (Objects.nonNull(cacheValue)) {
            return objectMapper.readValue(cacheValue, javaType);
        }

        String o;
        // 防止redis挂了以后db也访问不了
        try {
            o = redisTemplate.opsForValue().get(cacheKey);
        } catch (NestedRuntimeException e) {
            return pjp.proceed();
        }

        if (StringUtils.hasLength(o)) {
            return objectMapper.readValue(o, javaType);
        }

        String lock = LOCK + cacheKey;
        // 已经线程安全
        RLock rLock = redisson.getLock(lock);

        if (Boolean.FALSE.equals(rLock.tryLock(5000, TimeUnit.MILLISECONDS))) {
            throw new TimeoutException(GET_LOCK_TIMEOUT.getMsg());
        }

        Object proceed;

        try {
            // 双重检查
            String r = redisTemplate.opsForValue().get(cacheKey);

            if (StringUtils.hasLength(r)) {
                return objectMapper.readValue(r, javaType);
            }
            // 执行目标方法
            proceed = pjp.proceed();

            Cache annotation = method.getAnnotation(Cache.class);

            int expireTime = new Random().nextInt(annotation.expire());
            redisTemplate.opsForValue().set(cacheKey, objectMapper.writeValueAsString(proceed), expireTime, TimeUnit.MINUTES);
        } finally {
            rLock.unlock();
        }

        HashMap<String, Object> map = new HashMap<>();
        map.put(cacheKey, proceed);
        rabbitTemplate.convertAndSend(CacheRabbitConfig.CACHE_FANOUT_EXCHANGE, "", map);
        return proceed;

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
