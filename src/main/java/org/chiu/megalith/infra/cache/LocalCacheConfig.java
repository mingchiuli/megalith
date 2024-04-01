package org.chiu.megalith.infra.cache;

import java.util.Random;
import java.util.concurrent.TimeUnit;
import com.github.benmanes.caffeine.cache.Cache;

import com.github.benmanes.caffeine.cache.Expiry;
import org.checkerframework.checker.index.qual.NonNegative;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.github.benmanes.caffeine.cache.Caffeine;

@Configuration(proxyBeanMethods = false)
public class LocalCacheConfig {
    
    @Bean
    Cache<String, String> caffeineCache() {
        return Caffeine.newBuilder()
                .initialCapacity(128)// 初始大小
                .maximumSize(1024)// 最大数量
                .expireAfter(new Expiry<String, String>() {
                    @Override
                    public long expireAfterCreate(String key, String value, long currentTime) {
                        return TimeUnit.MINUTES.toNanos(new Random().nextInt(30));
                    }

                    @Override
                    public long expireAfterUpdate(String key, String value, long currentTime, @NonNegative long currentDuration) {
                        return 0;
                    }

                    @Override
                    public long expireAfterRead(String key, String value, long currentTime, @NonNegative long currentDuration) {
                        return -1;
                    }
                })//过期时间
                .build();
    }
}
