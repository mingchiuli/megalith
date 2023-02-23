package com.chiu.megalith.exhibit.scheduled;

import com.chiu.megalith.exhibit.controller.BlogController;
import com.chiu.megalith.exhibit.service.BlogService;
import com.chiu.megalith.common.lang.Const;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

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

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

    private static final String CACHE_FINISH_FLAG = "cache_finish_flag";

    @SneakyThrows
    @Scheduled(cron = "0 0 0/1 * * ?")
    public void configureTask() {

        RLock rLock = redisson.getLock("cacheKey");
        boolean locked = rLock.tryLock();

        if (!locked) {
            return;
        }

        try {
            if (Boolean.FALSE.equals(redisTemplate.hasKey(CACHE_FINISH_FLAG))) {
                long startMillis = System.currentTimeMillis();

                List<Integer> years = blogService.searchYears();
                CompletableFuture<Void> var1 = CompletableFuture.runAsync(() -> {
                    //getBlogDetail和getBlogStatus接口，分别考虑缓存和bloom
                    List<Long> idsUnlocked = blogService.findIdsByStatus(0);
                    List<Long> idsLocked = blogService.findIdsByStatus(1);
                    idsUnlocked.forEach(id -> {
                        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_BLOG.getInfo(), id, true);
                        blogController.getBlogDetail(id);
                        blogController.getBlogStatus(id);
                    });

                    idsLocked.forEach(id -> {
                        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_BLOG.getInfo(), id, true);
                        blogController.getBlogStatus(id);
                    });

                }, executor);

                CompletableFuture<Void> var2 = CompletableFuture.runAsync(() -> {
                    //listPage接口，分别考虑缓存和bloom
                    Long count = blogService.count();
                    int totalPage = (int) (count % blogPageSize == 0 ? count / blogPageSize : count / blogPageSize + 1);

                    for (int no = 1; no <= totalPage; no++) {
                        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_PAGE.getInfo(), no, true);
                        blogController.listPage(no);
                    }
                }, executor);


                CompletableFuture<Void> var3 = CompletableFuture.runAsync(() -> {
                    //getCountByYear接口
                    years.forEach(blogController::getCountByYear);
                }, executor);


                CompletableFuture<Void> var4 = CompletableFuture.runAsync(() -> {
                    //listByYear接口，分别考虑缓存和bloom
                    for (Integer year : years) {
                        //当前年份的总页数
                        Integer count = blogService.getCountByYear(year);
                        int totalPage = count % blogPageSize == 0 ? count / blogPageSize : count / blogPageSize + 1;

                        for (int no = 1; no <= totalPage; no++) {
                            redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_YEAR_PAGE.getInfo() + year, no, true);
                            blogController.listPageByYear(no, year);
                        }
                    }
                }, executor);


                //searchYears和getCountByYear
                CompletableFuture<Void> var5 = CompletableFuture.runAsync(() -> {
                    blogController.searchYears();
                    //getCountByYear的bloom和缓存
                    years.forEach(year -> {
                        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_YEARS.getInfo(), year, true);
                        blogController.getCountByYear(year);
                    });
                }, executor);

                CompletableFuture.allOf(var1, var2, var3, var4, var5).get();
                long endMillis = System.currentTimeMillis();
                redisTemplate.opsForValue().set(
                        CACHE_FINISH_FLAG,
                        CACHE_FINISH_FLAG,
                        10,
                        TimeUnit.MINUTES);

                log.info("定时任务执行用时{}毫秒", endMillis - startMillis);
            }
        } finally {
            rLock.unlock();
        }

    }

}
