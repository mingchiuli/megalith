package org.chiu.megalith.infra.schedule.task;

import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.infra.lang.Const;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.locks.LockSupport;

/**
 * @author mingchiuli
 * @create 2023-06-24 5:32 pm
 */
public record BlogsRunnable(Thread thread, 
                            PageMarker pageMarker,
                            ThreadPoolExecutor executor,
                            StringRedisTemplate redisTemplate,
                            BlogService blogService,
                            int batchPageTotal,
                            int totalPage) implements Runnable {

    @Override
    public void run() {
        int _curPageNo = 0;
        if (!pageMarker.fin) {
            synchronized (thread) {
                if (!pageMarker.fin) {
                    pageMarker.curPageNo++;
                    _curPageNo = pageMarker.curPageNo;

                    if (_curPageNo == batchPageTotal) {
                        pageMarker.fin = true;
                    }

                    if (thread.isInterrupted() && executor.getActiveCount() < executor.getMaximumPoolSize() >> 1) {
                        LockSupport.unpark(thread);
                    }
                }
            }

            if (_curPageNo > 0) {
                for (int no = (_curPageNo - 1) * 20 + 1; no <= (_curPageNo == batchPageTotal && totalPage % 20 != 0 ? totalPage : _curPageNo * 20); no++) {
                    redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_PAGE.getInfo(), no, true);
                    blogService.findPage(no, Integer.MIN_VALUE);
                }
            }
        }
    }
}
