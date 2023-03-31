package com.chiu.megalith.infra.bloom.handler.impl;


import com.chiu.megalith.infra.bloom.handler.BloomHandler;
import com.chiu.megalith.infra.exception.NotFoundException;
import com.chiu.megalith.infra.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class CountYearHandler extends BloomHandler {

    private final StringRedisTemplate redisTemplate;

    @Override
    public void handle(Object[] args) {
        Integer year = (Integer) args[0];
        if (Boolean.FALSE.equals(redisTemplate.opsForValue().getBit(Const.BLOOM_FILTER_YEARS.getInfo(), year))) {
            throw new NotFoundException("Not found " + year + " year");
        }
    }
}
