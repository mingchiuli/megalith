package org.chiu.megalith.infra.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.benmanes.caffeine.cache.Cache;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;

@Component
@RequiredArgsConstructor
@Slf4j
public class CacheMessageListener {

    private final Cache<String, String> localCache;

    private final ObjectMapper objectMapper;

    @SneakyThrows
    public void handleMessage(Map<String, Object> maps) {
        HashMap<String, String> values = new HashMap<>();

        for (Map.Entry<String, Object> entry : maps.entrySet()) {
            values.put(entry.getKey(), objectMapper.writeValueAsString(entry.getValue()));
        }
        localCache.putAll(values);
    }
}
