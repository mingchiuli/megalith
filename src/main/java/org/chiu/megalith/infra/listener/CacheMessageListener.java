package org.chiu.megalith.infra.listener;

import java.util.Set;

import org.springframework.stereotype.Component;
import com.github.benmanes.caffeine.cache.Cache;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Component
@RequiredArgsConstructor
@Slf4j
public class CacheMessageListener {

    private final Cache<String, Object> localCache;

    public void handleMessage(Set<String> keys) {
        localCache.invalidateAll(keys);
    }
}
