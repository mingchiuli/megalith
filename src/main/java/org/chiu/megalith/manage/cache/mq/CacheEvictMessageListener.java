package org.chiu.megalith.manage.cache.mq;

import java.util.Set;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import com.github.benmanes.caffeine.cache.Cache;
import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
@Slf4j
public class CacheEvictMessageListener {

    private final Cache<String, Object> localCache;

    @SneakyThrows
    public void handleMessage(Set<String> keys) {
        localCache.invalidateAll(keys);
    }
}
