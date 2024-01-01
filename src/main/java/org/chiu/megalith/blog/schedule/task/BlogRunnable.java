package org.chiu.megalith.blog.schedule.task;

import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.util.List;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2023-06-24 5:00 pm
 */
public record BlogRunnable (
        BlogService blogService,
        StringRedisTemplate redisTemplate,
        PageRequest pageRequest) implements Runnable {

    @Override
    public void run() {
        List<Long> idList = blogService.findIds(pageRequest);
        Optional.ofNullable(idList).ifPresent(ids ->
                ids.forEach(id -> {
                    redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_BLOG.getInfo(), id, true);
                    Integer status = blogService.findStatusById(id);
                    if (StatusEnum.NORMAL.getCode().equals(status)) {
                        blogService.findById(id, false);
                    } else if (StatusEnum.HIDE.getCode().equals(status)){
                        blogService.findById(id, true);
                    }
                })
        );
    }
}
