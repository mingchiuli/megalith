package org.chiu.megalith.infra.schedule.task;

import org.chiu.megalith.blog.controller.BlogController;
import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.infra.exception.NotFoundException;
import org.chiu.megalith.infra.lang.Const;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.locks.LockSupport;

/**
 * @author mingchiuli
 * @create 2023-06-24 5:00 pm
 */
public record BlogRunnable(ThreadPoolExecutor executor,
                           BlogController blogController,
                           BlogService blogService,
                           StringRedisTemplate redisTemplate,
                           Thread thread,
                           PageMarker pageMarker) implements Runnable {

    @Override
    public void run() {
        if (!pageMarker.fin) {
            List<Long> idList = null;
            synchronized (thread) {
                if (!pageMarker.fin) {
                    var pageRequest = PageRequest.of(pageMarker.curPageNo, 50);
                    idList = blogService.findIds(pageRequest);
                    int size = idList.size();
                    if (size < 50 && !pageMarker.fin) {
                        pageMarker.fin = true;
                    }
                    if (size == 50) {
                        pageMarker.curPageNo++;
                    }

                    if (thread.isInterrupted() && executor.getActiveCount() < executor.getMaximumPoolSize() >> 1) {
                        LockSupport.unpark(thread);
                    }
                }
            }

            Optional.ofNullable(idList).ifPresent(ids ->
                    ids.forEach(id -> {
                        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_BLOG.getInfo(), id, true);
                        try {
                            blogService.findById(id, false);
                        } catch (NotFoundException e) {
                            blogService.findById(id, true);
                        }
                        blogController.getBlogStatus(id);
                    }));
        }
    }
}
