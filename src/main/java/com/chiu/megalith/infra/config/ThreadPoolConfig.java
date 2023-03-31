package com.chiu.megalith.infra.config;

import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author mingchiuli
 * @create 2022-04-26 10:06 PM
 */
@Configuration(proxyBeanMethods = false)
@EnableConfigurationProperties(ThreadPoolConfigProperties.class)
public class ThreadPoolConfig {

    @Bean("scheduledThreadPoolExecutor")
    public ThreadPoolExecutor scheduledThreadPoolExecutor(ThreadPoolConfigProperties pool) {
        return new ThreadPoolExecutor(30,
                50,
                pool.getKeepAliveTime(), TimeUnit.SECONDS,
                new LinkedBlockingDeque<>(300),
                Executors.defaultThreadFactory(),
                new ThreadPoolExecutor.AbortPolicy());
    }

    @Bean("readCountThreadPoolExecutor")
    public ThreadPoolExecutor readCountThreadPoolExecutor(ThreadPoolConfigProperties pool) {
        return new ThreadPoolExecutor(pool.getCoreSize(),
                pool.getMaxSize(),
                pool.getKeepAliveTime(), TimeUnit.SECONDS,
                new LinkedBlockingDeque<>(1000),
                Executors.defaultThreadFactory(),
                new ThreadPoolExecutor.AbortPolicy());
    }
}
