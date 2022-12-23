package com.chiu.megalith.blog.bloom.handler.impl;


import com.chiu.megalith.blog.bloom.handler.BloomHandler;
import com.chiu.megalith.common.exception.NotFoundException;
import com.chiu.megalith.common.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class CountByYearBloomHandler implements BloomHandler {

    private final StringRedisTemplate redisTemplate;

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.equals(CountByYearBloomHandler.class);
    }

    @Override
    public void handle(Object[] args) {
        Integer year = (Integer) args[0];
        if (Boolean.FALSE.equals(redisTemplate.opsForValue().getBit(Const.BLOOM_FILTER_YEARS.getMsg(), year))) {
            throw new NotFoundException("Not found " + year + " year");
        }
    }
}
