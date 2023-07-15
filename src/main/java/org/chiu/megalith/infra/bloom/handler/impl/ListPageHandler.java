package org.chiu.megalith.infra.bloom.handler.impl;

import org.chiu.megalith.infra.bloom.handler.BloomHandler;
import org.chiu.megalith.infra.exception.NotFoundException;
import org.chiu.megalith.infra.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.util.Objects;

@Component
@RequiredArgsConstructor
public class ListPageHandler extends BloomHandler {

    private final StringRedisTemplate redisTemplate;

    @Override
    public void handle(Object[] args) {
        Integer currentPage = (Integer) args[0];
        Integer year = (Integer) args[1];

        if (Objects.equals(year, Integer.MIN_VALUE)) {
            if (Boolean.FALSE.equals(redisTemplate.opsForValue().getBit(Const.BLOOM_FILTER_PAGE.getInfo(), currentPage))) {
                throw new NotFoundException("Not found " + currentPage + " page");
            }
        } else {
            if (Boolean.FALSE.equals(redisTemplate.opsForValue().getBit(Const.BLOOM_FILTER_YEAR_PAGE.getInfo() + year, currentPage))) {
                throw new NotFoundException("Not found " + year + " year " + currentPage + " page");
            }
        }
    }
}
