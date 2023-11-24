package org.chiu.megalith.blog.schedule.task;

import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.infra.lang.Const;
import org.springframework.data.redis.core.StringRedisTemplate;

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
        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_PAGE.getInfo(), pageNo, true);
        blogService.findPage(pageNo, Integer.MIN_VALUE);
    }
}
