package org.chiu.megalith.infra.bloom.handler.impl;


import org.chiu.megalith.infra.bloom.handler.BloomHandler;
import org.chiu.megalith.infra.exception.NotFoundException;
import org.chiu.megalith.infra.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DetailHandler extends BloomHandler {

    private final StringRedisTemplate redisTemplate;

    @Override
    public void handle(Object[] args) {
        Long blogId = (Long) args[0];
        if (Boolean.FALSE.equals(redisTemplate.opsForValue().getBit(Const.BLOOM_FILTER_BLOG.getInfo(), blogId))) {
            throw new NotFoundException("Not found "+ blogId + " blog");
        }
    }
}
