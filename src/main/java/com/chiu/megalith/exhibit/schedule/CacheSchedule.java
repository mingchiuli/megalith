package com.chiu.megalith.exhibit.schedule;

import com.chiu.megalith.exhibit.controller.BlogController;
import com.chiu.megalith.exhibit.service.BlogService;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.manage.service.UserService;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import java.util.List;
import java.util.Optional;
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
    @SneakyThrows
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
                int maxPoolSize = executor.getMaximumPoolSize();
                CompletableFuture<Void> var1 = CompletableFuture.runAsync(() -> {
                    //getBlogDetail和getBlogStatus接口，分别考虑缓存和bloom
                    Thread thread = Thread.currentThread();
                    cacheAndBloomBlog(thread, 0);
                    cacheAndBloomBlog(thread, 1);
                }, executor);

                CompletableFuture<Void> var2 = CompletableFuture.runAsync(() -> {
                    //listPage接口，分别考虑缓存和bloom
                    Long count = blogService.count();
                    int totalPage = (int) (count % blogPageSize == 0 ? count / blogPageSize : count / blogPageSize + 1);

                    int batchPageTotal = totalPage % 20 == 0 ? totalPage / 20 : totalPage / 20 + 1;

                    Thread thread = Thread.currentThread();
                    var ref = new Object() {
                        volatile int curPageNo;
                        volatile boolean fin;
                    };

                    for (;;) {
                        if (ref.curPageNo <= batchPageTotal && maxPoolSize > executor.getActiveCount() && executor.getQueue().size() < 20) {
                            executor.execute(() -> {
                                int _curPageNo = 0;
                                if (!ref.fin) {
                                    synchronized (thread) {
                                        if (!ref.fin) {
                                            ref.curPageNo++;
                                            _curPageNo = ref.curPageNo;
                                        }
                                        if (_curPageNo == batchPageTotal) {
                                            ref.fin = true;
                                        }

                                        if (thread.isInterrupted() && executor.getActiveCount() < maxPoolSize >> 1) {
                                            LockSupport.unpark(thread);
                                        }
                                    }
                                }
                                if (_curPageNo > 0) {
                                    for (int no = (_curPageNo - 1) * 20 + 1; no <= (_curPageNo == batchPageTotal && totalPage % 20 != 0 ? totalPage : _curPageNo * 20); no++) {
                                        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_PAGE.getInfo(), no, true);
                                        blogController.listPage(no);
                                    }
                                }
                            });

                            if (ref.fin) {
                                break;
                            }

                        } else {
                            thread.interrupt();
                            LockSupport.park();
                            Thread.interrupted();
                        }
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
                        executor.execute(() -> {
                            int count = blogService.getCountByYear(year);
                            int totalPage = count % blogPageSize == 0 ? count / blogPageSize : count / blogPageSize + 1;

                            for (int no = 1; no <= totalPage; no++) {
                                redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_YEAR_PAGE.getInfo() + year, no, true);
                                blogController.listPageByYear(no, year);
                            }
                        });
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


                //unlock user
                CompletableFuture<Void> var6 = CompletableFuture.runAsync(() -> {
                    List<Long> ids = userService.findIdsByStatus(1);
                    ids.forEach(id -> userService.changeUserStatusById(id, 0));

                }, executor);

                CompletableFuture.allOf(var1, var2, var3, var4, var5, var6).get(5, TimeUnit.SECONDS);

                long endMillis = System.currentTimeMillis();
                redisTemplate.opsForValue().set(
                        CACHE_FINISH_FLAG,
                        "1",
                        5,
                        TimeUnit.SECONDS);

                log.info("定时任务执行用时{}毫秒", endMillis - startMillis);
            }
        } finally {
            rLock.unlock();
        }
    }

    private void cacheAndBloomBlog(Thread thread, Integer status) {

        var ref = new Object() {
            volatile boolean fin;
            volatile int currentPageMark;
        };

        int maxPoolSize = executor.getMaximumPoolSize();
        for (;;) {
            if (maxPoolSize > executor.getActiveCount() && executor.getQueue().size() < 20) {
                executor.execute(() -> {
                    if (!ref.fin) {
                        List<Long> list = null;
                        synchronized (thread) {
                            if (!ref.fin) {
                                Pageable pageRequest = PageRequest.of(ref.currentPageMark, 50);
                                list = blogService.findIdsByStatus(status, pageRequest);
                                int size = list.size();
                                if (size < 50 && !ref.fin) {
                                    ref.fin = true;
                                }
                                if (size == 50) {
                                    ref.currentPageMark++;
                                }

                                if (thread.isInterrupted() && executor.getActiveCount() < maxPoolSize >> 1) {
                                    LockSupport.unpark(thread);
                                }
                            }
                        }

                        Optional.ofNullable(list).ifPresent(ids ->
                                ids.forEach(id -> {
                                    redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_BLOG.getInfo(), id, true);
                                    if (status == 0) {
                                        blogService.findByIdAndStatus(id, 0);
                                    }
                                    blogController.getBlogStatus(id);
                        }));

                    }
                });
                if (ref.fin) {
                    break;
                }
            } else {
                thread.interrupt();
                LockSupport.park();
                Thread.interrupted();
            }
        }
    }

}
