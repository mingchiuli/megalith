package org.chiu.megalith.infra.statistic;

import jakarta.servlet.http.HttpServletRequest;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;

import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.core.annotation.Order;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.List;
import java.util.Objects;

import static org.chiu.megalith.infra.lang.Const.*;

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

    @Pointcut(value ="execution(* org.chiu.megalith.blog.controller.*.*(..)) || execution(* org.chiu.megalith.search.controller.*.*(..))")
    public void pt() {}

    //必须同步
    @SneakyThrows
    @Before("pt()")
    public void before() {
        HttpServletRequest request;
        try {
            request = ((ServletRequestAttributes) (RequestContextHolder.currentRequestAttributes())).getRequest();
        } catch (IllegalStateException e) {
            return;
        }
        String ipAddr = getIpAddr(request);
        redisTemplate.execute(LuaScriptUtils.statisticLua,
                List.of(DAY_VISIT.getInfo(), WEEK_VISIT.getInfo(), MONTH_VISIT.getInfo(), YEAR_VISIT.getInfo()),
                ipAddr);
    }

    private String getIpAddr(HttpServletRequest request) {
        // nginx代理获取的真实用户ip
        String ip = request.getHeader("X-Real-IP");
        if (!StringUtils.hasLength(ip) || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("X-Forwarded-For");
        }
        if (!StringUtils.hasLength(ip) || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("Proxy-Client-IP");
        }
        if (!StringUtils.hasLength(ip) || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("WL-Proxy-Client-IP");
        }
        if (!StringUtils.hasLength(ip) || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getRemoteAddr();
        }
        /*
          对于通过多个代理的情况， 第一个IP为客户端真实IP,多个IP按照','分割 "***.***.***.***".length() =
          15
         */
        if (Objects.nonNull(ip) && ip.length() > 15) {
            int idx = ip.indexOf(",");
            if (idx > 0) {
                ip = ip.substring(0, idx);
            }
        }
        return ip;
    }
}
