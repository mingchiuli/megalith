package com.chiu.megalith.infra.bloom;

import com.chiu.megalith.infra.bloom.handler.BloomHandler;
import com.chiu.megalith.infra.utils.SpringUtils;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.Signature;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.core.NestedRuntimeException;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.Map;
import java.util.Objects;

/**
 * @author mingchiuli
 * @create 2022-06-07 11:01 AM
 */
@Aspect
@Component
@Slf4j
@Order(1)
public class BloomAspect {

    private static class CacheHandlers {
        private static final Map<String, BloomHandler> cacheHandlers = SpringUtils.getHandlers(BloomHandler.class);
    }

    @Pointcut("@annotation(com.chiu.megalith.infra.bloom.Bloom)")
    public void pt() {}

    @SneakyThrows
    @Before("pt()")
    public void before(JoinPoint jp) {
        Signature signature = jp.getSignature();
        //方法名
        String methodName = signature.getName();
        //参数
        Object[] args = jp.getArgs();
        Class<?>[] classes = new Class[args.length];
        for (int i = 0; i < args.length; i++) {
            if (Objects.nonNull(args[i])) {
                classes[i] = args[i].getClass();
            }
        }

        Class<?> declaringType = signature.getDeclaringType();
        Method method = declaringType.getMethod(methodName, classes);
        Bloom bloom = method.getAnnotation(Bloom.class);
        Class<? extends BloomHandler> handler0 = bloom.handler();

        for (BloomHandler handler : CacheHandlers.cacheHandlers.values()) {
            if (handler.supports(handler0)) {
                try {
                    handler.handle(args);
                    break;
                } catch (NestedRuntimeException e) {
                    log.error(e.toString());
                }
            }
        }
    }
}
