package com.chiu.megalith.infra.bloom.handler.impl;

import com.chiu.megalith.infra.bloom.handler.BloomHandler;
import com.chiu.megalith.infra.exception.NotFoundException;
import com.chiu.megalith.infra.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

/**
 * 这个bloom和查DetailBloomHandler是一样的
 */
@Component
@RequiredArgsConstructor
public class BlogStatusHandler extends BloomHandler {

    private final StringRedisTemplate redisTemplate;

    @Override
    public void handle(Object[] args) {
        Long blogId = (Long) args[0];
        if (Boolean.FALSE.equals(redisTemplate.opsForValue().getBit(Const.BLOOM_FILTER_BLOG.getInfo(), blogId))) {
            throw new NotFoundException("Not found "+ blogId + " blog");
        }
    }
}
