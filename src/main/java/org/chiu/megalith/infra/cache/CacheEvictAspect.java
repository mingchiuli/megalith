package org.chiu.megalith.infra.cache;

import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.Signature;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.chiu.megalith.infra.config.CacheEvictRabbitConfig;
import org.chiu.megalith.infra.lang.Const;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.core.annotation.Order;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;

@Aspect
@Component
@Order(3)
@RequiredArgsConstructor
public class CacheEvictAspect {

    private final StringRedisTemplate redisTemplate;

    private final RabbitTemplate rabbitTemplate;

    @Pointcut("@annotation(org.chiu.megalith.infra.cache.CacheEvict)")
    public void pt() {}

    @SneakyThrows
    @After("pt()")
    @Async("commonExecutor")
    public void around(JoinPoint jp) {
        Signature signature = jp.getSignature();
        String methodName = signature.getName();
        Class<?> declaringType = signature.getDeclaringType();

        var parameterTypes = new Class[jp.getArgs().length];
        Object[] args = jp.getArgs();
        for (int i = 0; i < args.length; i++) {
            parameterTypes[i] = args[i].getClass();
        }

        Method method = declaringType.getMethod(methodName, parameterTypes);
        var annotation = method.getAnnotation(CacheEvict.class);
        Const[] prefix = annotation.prefix();

        for (Const key : prefix) {
            Set<String> keys = Optional.ofNullable(redisTemplate.keys(key.getInfo() + "*"))
                    .orElseGet(LinkedHashSet::new);
            if (!keys.isEmpty()) {
                redisTemplate.delete(keys);
                rabbitTemplate.convertAndSend(CacheEvictRabbitConfig.CACHE_EVICT_FANOUT_EXCHANGE, "", keys);
            }
        }
    }
}
