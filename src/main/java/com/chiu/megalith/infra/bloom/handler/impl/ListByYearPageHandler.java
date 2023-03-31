package com.chiu.megalith.infra.bloom.handler.impl;


import com.chiu.megalith.infra.bloom.handler.BloomHandler;
import com.chiu.megalith.infra.exception.NotFoundException;
import com.chiu.megalith.infra.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class ListByYearPageHandler extends BloomHandler {

    private final StringRedisTemplate redisTemplate;

    @Override
    public void handle(Object[] args) {
        Integer currentPage = (Integer) args[0];
        Integer yearMark = (Integer) args[1];
        if (Boolean.FALSE.equals(redisTemplate.opsForValue().getBit(Const.BLOOM_FILTER_YEAR_PAGE.getInfo() + yearMark, currentPage))) {
            throw new NotFoundException("Not found " + yearMark + " year" + currentPage + " page");
        }
    }
}
