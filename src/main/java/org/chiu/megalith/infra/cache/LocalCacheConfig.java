package org.chiu.megalith.infra.cache;

import java.util.concurrent.TimeUnit;
import com.github.benmanes.caffeine.cache.Cache;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.github.benmanes.caffeine.cache.Caffeine;

@Configuration(proxyBeanMethods = false)
public class LocalCacheConfig {
    
    @Bean
    Cache<String, Object> caffeineCache() {
        return Caffeine.newBuilder()
                .initialCapacity(128)// 初始大小
                .maximumSize(1024)// 最大数量
                .expireAfterWrite(60, TimeUnit.MINUTES)// 过期时间
                .build();
    }
}
