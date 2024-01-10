package org.chiu.megalith.infra.bloom.handler.impl;


import org.chiu.megalith.infra.bloom.handler.BloomHandler;
import org.chiu.megalith.infra.exception.MissException;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import static org.chiu.megalith.infra.lang.Const.BLOOM_FILTER_BLOG;
import static org.chiu.megalith.infra.lang.ExceptionMessage.NO_FOUND;

@Component
@RequiredArgsConstructor
public class DetailHandler extends BloomHandler {

    private final StringRedisTemplate redisTemplate;

    @Override
    public void handle(Object[] args) {
        Long blogId = (Long) args[0];
        Boolean bit = redisTemplate.opsForValue().getBit(BLOOM_FILTER_BLOG.getInfo(), blogId);
        if (Boolean.FALSE.equals(bit)) {
            throw new MissException(NO_FOUND.getMsg() + blogId + " blog");
        }
    }
}
