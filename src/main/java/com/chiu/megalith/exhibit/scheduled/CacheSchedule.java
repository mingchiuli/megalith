package com.chiu.megalith.exhibit.scheduled;

import com.chiu.megalith.exhibit.entity.BlogEntity;
import com.chiu.megalith.exhibit.service.BlogService;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.lang.Result;
import com.chiu.megalith.common.page.PageAdapter;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ThreadLocalRandom;
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

    private final StringRedisTemplate redisTemplate;

    private final ObjectMapper objectMapper;

    private final RedissonClient redisson;

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

    private static final String CACHE_FINISH_FLAG = "cache_finish_flag";


//    getBlogDetail: 通过id查博客细节
//    listPage：全体博客，查当前页的摘要
//    listPageByYear：年份博客，查当前页的摘要
//    getCountByYear：计算当前年份的博客数量
//    getBlogStatus：当前博客是否可见
    @SneakyThrows
    @Scheduled(cron = "0 0 0/2 * * ?")
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
                    Long count = blogService.count();
                    long totalPage = count % 30 == 0 ? count / 30 : count / 30 + 1;

                    for (int no = 1; no <= totalPage; no++) {
                        Pageable pageRequest = PageRequest.of(no - 1,
                                30,
                                Sort.by("created").descending());

                        PageAdapter<BlogEntity> pageAdapter = blogService.listPageCustom(pageRequest);
                        List<BlogEntity> allBlogs = pageAdapter.getContent();
                        List<BlogEntity> blogs = allBlogs.
                                stream().
                                filter(blog -> blog.getStatus() == 0).
                                toList();

                        blogs.forEach(blog -> {

                            StringBuilder builder = new StringBuilder();
                            builder.append("::").append(blog.getId());
                            String contentPrefix = Const.HOT_BLOG + "::BlogServiceImpl::getBlogDetail" + builder;
                            String statusPrefix = Const.BLOG_STATUS + "::BlogController::getBlogStatus" + builder;

                            try {
                                redisTemplate.opsForValue().set(contentPrefix, objectMapper.writeValueAsString(blog),
                                        ThreadLocalRandom.current().nextInt(120) + 1,
                                        TimeUnit.MINUTES);
                                redisTemplate.opsForValue().set(statusPrefix, objectMapper.writeValueAsString(Result.success(blog.getStatus())),
                                        ThreadLocalRandom.current().nextInt(120) + 1,
                                        TimeUnit.MINUTES);
                            } catch (JsonProcessingException e) {
                                log.info(e.getMessage());
                            }
                        });

                        //bloomFilter
                        allBlogs.forEach(blog -> redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_BLOG.getInfo(), blog.getId(), true));
                    }

                }, executor);

                CompletableFuture<Void> var2 = CompletableFuture.runAsync(() -> {
                    //listPage接口，分别考虑缓存和bloom
                    Long count = blogService.count();
                    int totalPage = (int) (count % blogPageSize == 0 ? count / blogPageSize : count / blogPageSize + 1);

                    for (int no = 1; no <= totalPage; no++) {
                        PageAdapter<BlogEntity> page = blogService.listPage(no);
                        String pagesPrefix = Const.HOT_BLOGS + "::BlogController::listPage" + "::" + no;
                        try {
                            redisTemplate.opsForValue().set(pagesPrefix,
                                    objectMapper.writeValueAsString(Result.success(page)),
                                    ThreadLocalRandom.current().nextInt(120) + 1,
                                    TimeUnit.MINUTES);
                        } catch (JsonProcessingException e) {
                            log.info(e.getMessage());
                        }
                        //bloomFilter
                        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_PAGE.getInfo(), no, true);
                    }
                }, executor);


                CompletableFuture<Void> var3 = CompletableFuture.runAsync(() -> {
                    //getCountByYear接口
                    years.forEach(year -> {
                        Integer countYear = blogService.getCountByYear(year);
                        String yearCountPrefix = Const.HOT_BLOGS + "::BlogController::getCountByYear" + "::" + year;
                        try {
                            redisTemplate.opsForValue().set(yearCountPrefix,
                                    objectMapper.writeValueAsString(Result.success(countYear)),
                                    ThreadLocalRandom.current().nextInt(120) + 1,
                                    TimeUnit.MINUTES);
                        } catch (JsonProcessingException e) {
                            log.info(e.getMessage());
                        }
                    });
                }, executor);


                CompletableFuture<Void> var4 = CompletableFuture.runAsync(() -> {
                    //listByYear接口，分别考虑缓存和bloom
                    for (Integer year : years) {
                        //当前年份的总页数
                        Integer count = blogService.getCountByYear(year);
                        int totalPage = count % blogPageSize == 0 ? count / blogPageSize : count / blogPageSize + 1;

                        for (int no = 1; no <= totalPage; no++) {
                            //每一页的缓存
                            PageAdapter<BlogEntity> page = blogService.listPageByYear(no, year);
                            String yearListPrefix = Const.HOT_BLOGS + "::BlogController::listPageByYear" + "::" + no + "::" + year;
                            try {
                                redisTemplate.opsForValue().set(yearListPrefix, objectMapper.writeValueAsString(Result.success(page)), ThreadLocalRandom.current().nextInt(120) + 1, TimeUnit.MINUTES);
                            } catch (JsonProcessingException e) {
                                log.info(e.getMessage());
                            }
                            //bloom过滤器
                            redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_YEAR_PAGE.getInfo() + year, no, true);
                        }
                    }
                }, executor);


                //searchYears和getCountByYear
                CompletableFuture<Void> var5 = CompletableFuture.runAsync(() -> {
                    String yearKey = Const.YEARS + "::BlogController::searchYears";
                    try {
                        redisTemplate.opsForValue().set(yearKey,
                                objectMapper.writeValueAsString(Result.success(years)),
                                ThreadLocalRandom.current().nextInt(120) + 1,
                                TimeUnit.MINUTES);
                    } catch (JsonProcessingException e) {
                        log.info(e.getMessage());
                    }
                    //getCountByYear的bloom和缓存
                    years.forEach(year -> {
                        String countKey = Const.HOT_BLOGS + "::BlogController::getCountByYear::" + year;
                        Integer count = blogService.getCountByYear(year);
                        try {
                            redisTemplate.opsForValue().set(countKey,
                                    objectMapper.writeValueAsString(Result.success(count)),
                                    ThreadLocalRandom.current().nextInt(120) + 1,
                                    TimeUnit.MINUTES);
                        } catch (JsonProcessingException e) {
                            log.info(e.getMessage());
                        }
                        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_YEARS.getInfo(), year, true);
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
