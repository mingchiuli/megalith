package org.chiu.megalith.infra.cache;

import com.github.benmanes.caffeine.cache.Expiry;
import org.checkerframework.checker.index.qual.NonNegative;

import java.util.Random;
import java.util.concurrent.TimeUnit;

public class LocalCacheExpiry implements Expiry<String, Object> {
    @Override
    public long expireAfterCreate(String key, Object value, long currentTime) {
        return TimeUnit.MINUTES.toNanos(new Random().nextInt(30));
    }

    @Override
    public long expireAfterUpdate(String key, Object value, long currentTime, @NonNegative long currentDuration) {
        return currentDuration;
    }

    @Override
    public long expireAfterRead(String key, Object value, long currentTime, @NonNegative long currentDuration) {
        return currentDuration;
    }
}
