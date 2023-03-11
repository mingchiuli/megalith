package com.chiu.megalith.exhibit.bloom.handler.impl;


import com.chiu.megalith.exhibit.bloom.handler.BloomHandler;
import com.chiu.megalith.base.exception.NotFoundException;
import com.chiu.megalith.base.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DetailPageHandler extends BloomHandler {

    private final StringRedisTemplate redisTemplate;

    @Override
    public void handle(Object[] args) {
        Long blogId = (Long) args[0];
        if (Boolean.FALSE.equals(redisTemplate.opsForValue().getBit(Const.BLOOM_FILTER_BLOG.getInfo(), blogId))) {
            throw new NotFoundException("Not found "+ blogId + " blog");
        }
    }
}
