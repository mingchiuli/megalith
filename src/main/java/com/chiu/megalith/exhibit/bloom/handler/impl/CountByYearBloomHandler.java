package com.chiu.megalith.exhibit.bloom.handler.impl;


import com.chiu.megalith.exhibit.bloom.handler.BloomHandler;
import com.chiu.megalith.base.exception.NotFoundException;
import com.chiu.megalith.base.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class CountByYearBloomHandler implements BloomHandler {

    private final StringRedisTemplate redisTemplate;

    @Override
    public boolean supports(Class<? extends BloomHandler> clazz) {
        return clazz.equals(CountByYearBloomHandler.class);
    }

    @Override
    public void handle(Object[] args) {
        Integer year = (Integer) args[0];
        if (Boolean.FALSE.equals(redisTemplate.opsForValue().getBit(Const.BLOOM_FILTER_YEARS.getInfo(), year))) {
            throw new NotFoundException("Not found " + year + " year");
        }
    }
}
