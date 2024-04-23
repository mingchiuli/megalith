package org.chiu.megalith.manage.cache;

import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.Signature;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.core.annotation.Order;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.*;

import static org.chiu.megalith.infra.lang.Const.CACHE_EVICT_FANOUT_EXCHANGE;

@Aspect
@Component
@Order(3)
@RequiredArgsConstructor
public class CacheEvictAspect {

    private final StringRedisTemplate redisTemplate;

    private final RabbitTemplate rabbitTemplate;

    private final List<CacheEvictHandler> cacheEvictHandlers;

    @Pointcut("@annotation(org.chiu.megalith.manage.cache.CacheEvict)")
    public void pt() {}

    @SneakyThrows
    @Around("pt()")
    @Async("commonExecutor")
    public void around(ProceedingJoinPoint pjp) {
        Signature signature = pjp.getSignature();
        String methodName = signature.getName();
        Class<?> declaringType = signature.getDeclaringType();

        var parameterTypes = new Class[pjp.getArgs().length];
        Object[] args = pjp.getArgs();
        for (int i = 0; i < args.length; i++) {
            parameterTypes[i] = args[i].getClass();
        }

        Method method = declaringType.getMethod(methodName, parameterTypes);
        var annotation = method.getAnnotation(CacheEvict.class);
        Class<? extends CacheEvictHandler>[] handlers = annotation.handler();

        Set<String> keys = new HashSet<>();

        for (Class<? extends CacheEvictHandler> h : handlers) {
            for (CacheEvictHandler handler : cacheEvictHandlers) {
                if (handler.match(h)) {
                    Set<String> keySet = handler.handle(args);
                    keys.addAll(keySet);
                    break;
                }
            }
        }

        pjp.proceed();

        if (!keys.isEmpty()) {
            redisTemplate.delete(keys);
            rabbitTemplate.convertAndSend(CACHE_EVICT_FANOUT_EXCHANGE.getInfo(), "", keys);
        }
    }
}
