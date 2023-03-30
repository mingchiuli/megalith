package com.chiu.megalith.base.statistic;

import com.chiu.megalith.base.lang.Const;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.core.annotation.Order;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.stereotype.Component;

import java.net.InetAddress;
import java.util.Arrays;

/**
 * @author mingchiuli
 * @create 2023-03-29 11:32 pm
 */
@Aspect
@Component
@Slf4j
@Order(0)
@RequiredArgsConstructor
public class StatisticsAspect {

    private final StringRedisTemplate redisTemplate;

    private final RedisScript<Void> statisticLua = RedisScript.of(
            "redis.call('pfadd', KEYS[1], ARGV[1]);" +
            "redis.call('pfadd', KEYS[2], ARGV[1]);" +
            "redis.call('pfadd', KEYS[3], ARGV[1]);" +
            "redis.call('pfadd', KEYS[4], ARGV[1]);");

    @Pointcut(value ="execution(* com.chiu.megalith.exhibit.controller.*.*(..)) || execution(* com.chiu.megalith.search.controller.*.*(..))")
    public void pt() {}

    @SneakyThrows
    @Before("pt()")
    public void before() {
        String addr = InetAddress.getLocalHost().getHostAddress();

        redisTemplate.execute(statisticLua,
                Arrays.asList(Const.DAY_VISIT.getInfo(), Const.WEEK_VISIT.getInfo(), Const.MONTH_VISIT.getInfo(), Const.YEAR_VISIT.getInfo()),
                addr);
    }


}
