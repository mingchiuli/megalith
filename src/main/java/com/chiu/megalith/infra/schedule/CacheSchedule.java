package com.chiu.megalith.infra.schedule;

import com.chiu.megalith.blog.controller.BlogController;
import com.chiu.megalith.blog.service.BlogService;
import com.chiu.megalith.infra.lang.Const;
import com.chiu.megalith.infra.schedule.task.BlogRunnable;
import com.chiu.megalith.infra.schedule.task.BlogsRunnable;
import com.chiu.megalith.infra.schedule.task.PageMarker;
import com.chiu.megalith.manage.service.UserService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.*;
import java.util.concurrent.locks.LockSupport;

/**
 * @author mingchiuli
 * @create 2022-11-29 10:52 pm
 */
@Component
@Slf4j
@RequiredArgsConstructor
public class CacheSchedule {

    @Qualifier("scheduledThreadPoolExecutor")
    private final ThreadPoolExecutor executor;

    private final BlogService blogService;

    private final BlogController blogController;

    private final StringRedisTemplate redisTemplate;

    private final RedissonClient redisson;

    private final UserService userService;

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

    private static final String CACHE_FINISH_FLAG = "cache_finish_flag";

    @Scheduled(cron = "0 0 0/2 * * ?")
    public void configureTask() {

        RLock rLock = redisson.getLock("cacheKey");
        if (Boolean.FALSE.equals(rLock.tryLock())) {
            return;
        }

        try {
            Boolean executed = redisTemplate.hasKey(CACHE_FINISH_FLAG);
            if (Boolean.FALSE.equals(executed)) {
                exec();
                redisTemplate.opsForValue().set(CACHE_FINISH_FLAG, "flag", 119, TimeUnit.MINUTES);
            }
        } finally {
            rLock.unlock();
        }
    }


    @SuppressWarnings("all")
    private void exec() {
        List<Integer> years = blogService.searchYears();
        int maxPoolSize = executor.getMaximumPoolSize();
        //getBlogDetail和getBlogStatus接口，分别考虑缓存和bloom
        CompletableFuture.runAsync(() -> {
            var thread = Thread.currentThread();
            var pageMarker = new PageMarker();

            for (;;) {
                if (executor.getMaximumPoolSize() > executor.getActiveCount()) {
                    var runnable = new BlogRunnable(executor, blogController, blogService, redisTemplate, thread, pageMarker);
                    executor.execute(runnable);

                    if (pageMarker.fin) {
                        break;
                    }
                } else {
                    thread.interrupt();
                    LockSupport.park();
                    Thread.interrupted();
                }
            }
        }, executor);

        CompletableFuture.runAsync(() -> {
            //listPage接口，分别考虑缓存和bloom
            Long count = blogService.count();
            int totalPage = (int) (count % blogPageSize == 0 ? count / blogPageSize : count / blogPageSize + 1);
            int batchPageTotal = totalPage % 20 == 0 ? totalPage / 20 : totalPage / 20 + 1;
            var thread = Thread.currentThread();
            var pageMarker = new PageMarker();

            for (;;) {
                if (maxPoolSize > executor.getActiveCount()) {
                    var runnable = new BlogsRunnable(thread, pageMarker, executor, redisTemplate, blogService, batchPageTotal, totalPage);
                    executor.execute(runnable);

                    if (pageMarker.fin) {
                        break;
                    }
                } else {
                    thread.interrupt();
                    LockSupport.park();
                    Thread.interrupted();
                }
            }
        }, executor);

        CompletableFuture.runAsync(() -> {
            //listByYear接口，分别考虑缓存和bloom
            for (Integer year : years) {
                //当前年份的总页数
                executor.execute(() -> {
                    int count = blogService.getCountByYear(year);
                    int totalPage = count % blogPageSize == 0 ? count / blogPageSize : count / blogPageSize + 1;

                    for (int no = 1; no <= totalPage; no++) {
                        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_YEAR_PAGE.getInfo() + year, no, true);
                        blogService.findPage(no, year);
                    }
                });
            }

        }, executor);

        //searchYears和getCountByYear
        CompletableFuture.runAsync(() -> {
            blogController.searchYears();
            //getCountByYear的bloom和缓存
            years.forEach(year -> {
                redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_YEARS.getInfo(), year, true);
                blogService.getCountByYear(year);
            });

        }, executor);

        //unlock user & del statistic & del hot read
        CompletableFuture.runAsync(() -> {
            List<Long> ids = userService.findIdsByStatus(1);
            ids.forEach(id -> userService.changeUserStatusById(id, 0));
            var now = LocalDateTime.now();

            int hourOfDay = now.getHour();
            int dayOfWeek = now.getDayOfWeek().getValue();
            int dayOfMonth = now.getDayOfMonth();
            int dayOfYear = now.getDayOfYear();

            if (hourOfDay == 0) {
                redisTemplate.delete(Const.DAY_VISIT.getInfo());
                if (dayOfWeek == 1) {
                    redisTemplate.delete(Const.WEEK_VISIT.getInfo());
                    redisTemplate.unlink(Const.HOT_READ.getInfo());
                }
                if (dayOfMonth == 1) {
                    redisTemplate.delete(Const.MONTH_VISIT.getInfo());
                }
                if (dayOfYear == 1) {
                    redisTemplate.delete(Const.YEAR_VISIT.getInfo());
                }
            }
        }, executor);
    }
}
