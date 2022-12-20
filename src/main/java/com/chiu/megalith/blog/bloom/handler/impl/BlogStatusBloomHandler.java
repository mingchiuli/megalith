package com.chiu.megalith.blog.bloom.handler.impl;

import com.chiu.megalith.blog.bloom.handler.BloomHandler;
import com.chiu.megalith.common.exception.NotFoundException;
import com.chiu.megalith.common.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class BlogStatusBloomHandler implements BloomHandler {

    private final StringRedisTemplate redisTemplate;

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.equals(BlogStatusBloomHandler.class);
    }

    @Override
    public void handle(Object[] args) {
        Long blogId = (Long) args[0];
        if (Boolean.FALSE.equals(redisTemplate.opsForValue().getBit(Const.BLOOM_FILTER_BLOG.getMsg(), blogId))) {
            throw new NotFoundException("Not found "+ blogId + "blog");
        }
    }
}
