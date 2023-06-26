package com.chiu.megalith.infra.statistic;

import com.chiu.megalith.infra.lang.Const;
import com.chiu.megalith.infra.utils.LuaScriptUtils;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.core.annotation.Order;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.net.InetAddress;
import java.util.List;

/**
 * @author mingchiuli
 * @create 2023-03-29 11:32 pm
 */
@Aspect
@Component
@Order(0)
@RequiredArgsConstructor
public class StatisticsAspect {

    private final StringRedisTemplate redisTemplate;

    @Pointcut(value ="execution(* com.chiu.megalith.blog.controller.*.*(..)) || execution(* com.chiu.megalith.search.controller.*.*(..))")
    public void pt() {}

    @SneakyThrows
    @Before("pt()")
    public void before() {
        String addr = InetAddress.getLocalHost().getHostAddress();
        redisTemplate.execute(LuaScriptUtils.statisticLua,
                List.of(Const.DAY_VISIT.getInfo(), Const.WEEK_VISIT.getInfo(), Const.MONTH_VISIT.getInfo(), Const.YEAR_VISIT.getInfo()), addr);
    }
}
