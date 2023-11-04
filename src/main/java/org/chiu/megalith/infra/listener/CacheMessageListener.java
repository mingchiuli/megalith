package org.chiu.megalith.infra.listener;

import java.util.Set;

import org.springframework.stereotype.Component;
import com.github.benmanes.caffeine.cache.Cache;
import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class CacheMessageListener {

    private final Cache<String, Object> localCache;

    public void handleMessage(Set<String> keys) {
        localCache.invalidateAll(keys);
    }
}
