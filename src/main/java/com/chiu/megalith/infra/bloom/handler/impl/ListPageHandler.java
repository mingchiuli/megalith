package com.chiu.megalith.infra.bloom.handler.impl;


import com.chiu.megalith.infra.bloom.handler.BloomHandler;
import com.chiu.megalith.infra.exception.NotFoundException;
import com.chiu.megalith.infra.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class ListPageHandler extends BloomHandler {

    private final StringRedisTemplate redisTemplate;

    @Override
    public void handle(Object[] args) {
        Integer currentPage = (Integer) args[0];
        Integer year = (Integer) args[1];

        Optional.ofNullable(year).ifPresentOrElse(y -> {
            if (Boolean.FALSE.equals(redisTemplate.opsForValue().getBit(Const.BLOOM_FILTER_YEAR_PAGE.getInfo() + y, currentPage))) {
                throw new NotFoundException("Not found " + y + " year" + currentPage + " page");
            }
        }, () -> {
            if (Boolean.FALSE.equals(redisTemplate.opsForValue().getBit(Const.BLOOM_FILTER_PAGE.getInfo(), currentPage))) {
                throw new NotFoundException("Not found " + currentPage + " page");
            }
        });
    }
}
