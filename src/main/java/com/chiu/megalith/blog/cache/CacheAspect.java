package com.chiu.megalith.blog.cache;

import com.chiu.megalith.common.utils.RedisUtils;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.Signature;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.core.NestedRuntimeException;
import org.springframework.core.annotation.Order;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

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

    private static final String LOCK = "lock:";

    private final StringRedisTemplate redisTemplate;

    private final ObjectMapper objectMapper;

    private final RedisUtils rLock;

    @Pointcut("@annotation(com.chiu.megalith.blog.cache.Cache)")
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
            Optional<Object> arg = Optional.ofNullable(args[i]);
            if (arg.isPresent()) {
                //方法的参数必须是能够json化的
                params.append("::");
                if (arg.get() instanceof String) {
                    params.append(arg.get());
                } else {
                    params.append(objectMapper.writeValueAsString(arg.get()));
                }
                parameterTypes[i] = arg.get().getClass();
            }
        }

        Class<?> declaringType = signature.getDeclaringType();
        Method method = declaringType.getMethod(methodName, parameterTypes);

        Cache annotation = method.getAnnotation(Cache.class);
        long expire = annotation.expire();
        String prefix = annotation.prefix().getMsg();

        Type genericReturnType = method.getGenericReturnType();

        JavaType javaType;

        if (genericReturnType instanceof ParameterizedType parameterizedType) {
            javaType = getTypesReference(parameterizedType);
        } else {
            javaType = objectMapper.getTypeFactory().constructType(genericReturnType);
        }

        String redisKey = StringUtils.hasLength(prefix) ? prefix + "::" + className + "::" + methodName + params : className + "::" + methodName + params;

        Object o;

        //防止redis挂了以后db也访问不了
        try {
            o = redisTemplate.opsForValue().get(redisKey);
        } catch (NestedRuntimeException e) {
            return pjp.proceed();
        }


        if (o != null) {
            return objectMapper.convertValue(o, javaType);
        }

        String lock = (LOCK + className + methodName + params);


        //防止缓存击穿
        String uuid = UUID.randomUUID().toString();
        boolean locked = rLock.tryLock(lock, uuid, 5000, 5000);

        if (!locked) {
            return around(pjp);
        }

        try {
            //双重检查
            Object r = redisTemplate.opsForValue().get(redisKey);

            if (r != null) {
                return objectMapper.convertValue(r, javaType);
            }
            //执行目标方法
            Object proceed = pjp.proceed();
            redisTemplate.opsForValue().set(redisKey, objectMapper.writeValueAsString(proceed), expire, TimeUnit.SECONDS);
            return proceed;
        } finally {
            rLock.unLock(lock, uuid);
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
}
