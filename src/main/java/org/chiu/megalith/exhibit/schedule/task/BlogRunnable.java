package org.chiu.megalith.exhibit.schedule.task;

import org.chiu.megalith.exhibit.service.BlogService;
import org.chiu.megalith.exhibit.wrapper.BlogWrapper;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.util.List;
import java.util.Optional;

import static org.chiu.megalith.infra.lang.Const.BLOOM_FILTER_BLOG;

/**
 * @author mingchiuli
 * @create 2023-06-24 5:00 pm
 */
public record BlogRunnable (
        BlogService blogService,
        BlogWrapper blogWrapper,
        StringRedisTemplate redisTemplate,
        PageRequest pageRequest) implements Runnable {

    @Override
    public void run() {
        List<Long> idList = blogService.findIds(pageRequest);
        Optional.ofNullable(idList).ifPresent(ids ->
                ids.forEach(id -> {
                    redisTemplate.opsForValue().setBit(BLOOM_FILTER_BLOG.getInfo(), id, true);
                    blogWrapper.findStatusById(id);
                    blogWrapper.findById(id);
                })
        );
    }
}
