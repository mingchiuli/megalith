package org.chiu.megalith.blog.schedule.task;

import org.chiu.megalith.blog.service.BlogService;
import org.springframework.data.redis.core.StringRedisTemplate;

import static org.chiu.megalith.infra.lang.Const.BLOOM_FILTER_PAGE;

/**
 * @author mingchiuli
 * @create 2023-06-24 5:32 pm
 */
public record BlogsRunnable(
                            StringRedisTemplate redisTemplate,
                            BlogService blogService,
                            Integer pageNo) implements Runnable {

    @Override
    public void run() {
        redisTemplate.opsForValue().setBit(BLOOM_FILTER_PAGE.getInfo(), pageNo, true);
        blogService.findPage(pageNo, Integer.MIN_VALUE);
    }
}
