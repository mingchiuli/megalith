package org.chiu.megalith.exhibit.cache.mq;

import com.github.benmanes.caffeine.cache.Cache;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Set;

@Component
@RequiredArgsConstructor
@Slf4j
public class CacheBlogEvictMessageListener {

    private final Cache<String, Object> localCache;

    @SneakyThrows
    public void handleMessage(Set<String> keys) {
        localCache.invalidateAll(keys);
    }
}
