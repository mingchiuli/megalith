package org.chiu.megalith.blog.schedule;

import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.blog.wrapper.BlogWrapper;
import org.chiu.megalith.blog.schedule.task.BlogRunnable;
import org.chiu.megalith.blog.schedule.task.BlogsRunnable;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.manage.req.UserEntityReq;
import org.chiu.megalith.manage.service.UserService;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.vo.UserEntityVo;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.*;

import static org.chiu.megalith.infra.lang.Const.*;

/**
 * @author mingchiuli
 * @create 2022-11-29 10:52 pm
 */
@Component
@RequiredArgsConstructor
public class CacheSchedule {

    @Qualifier("commonExecutor")
    private final ExecutorService taskExecutor;

    private final BlogService blogService;

    private final BlogWrapper blogWrapper;

    private final StringRedisTemplate redisTemplate;

    private final RedissonClient redisson;

    private final UserService userService;

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

    private static final String CACHE_FINISH_FLAG = "cache_finish_flag";

    @Scheduled(cron = "0 0 0/1 * * ?")
    public void configureTask() {

        RLock rLock = redisson.getLock("cacheKey");
        if (Boolean.FALSE.equals(rLock.tryLock())) {
            return;
        }

        try {
            Boolean executed = redisTemplate.hasKey(CACHE_FINISH_FLAG);
            if (Boolean.FALSE.equals(executed)) {
                exec();
                redisTemplate.opsForValue().set(CACHE_FINISH_FLAG, "flag", 60, TimeUnit.SECONDS);
            }
        } finally {
            rLock.unlock();
        }
    }

    private void exec() {
        List<Integer> years = blogService.getYears();
        Long count = blogService.count();
        // getBlogDetail和getBlogStatus接口，分别考虑缓存和bloom
        CompletableFuture.runAsync(() -> {
            int pageSize = 20;
            int totalPage = (int) (count % pageSize == 0 ? count / pageSize : count / pageSize + 1);
            for (int i = 1; i <= totalPage; i++) {
                var runnable = new BlogRunnable(blogService, blogWrapper, redisTemplate, PageRequest.of(i, pageSize));
                taskExecutor.execute(runnable);
            }
        }, taskExecutor);

        CompletableFuture.runAsync(() -> {
            // listPage接口，分别考虑缓存和bloom
            int totalPage = (int) (count % blogPageSize == 0 ? 
                    count / blogPageSize : 
                    count / blogPageSize + 1);
            for (int i = 1; i <= totalPage; i++) {
                var runnable = new BlogsRunnable(redisTemplate, blogService, i);
                taskExecutor.execute(runnable);
            }
        }, taskExecutor);

        CompletableFuture.runAsync(() -> {
            // listByYear接口，分别考虑缓存和bloom
            for (Integer year : years) {
                // 当前年份的总页数
                taskExecutor.execute(() -> {
                    int countByYear = blogService.getCountByYear(year);
                    int totalPage = countByYear % blogPageSize == 0 ? 
                            countByYear / blogPageSize : 
                            countByYear / blogPageSize + 1;

                    for (int no = 1; no <= totalPage; no++) {
                        redisTemplate.opsForValue().setBit(BLOOM_FILTER_YEAR_PAGE.getInfo() + year, no, true);
                        blogService.findPage(no, year);
                    }
                });
            }

        }, taskExecutor);

        // searchYears和getCountByYear
        CompletableFuture.runAsync(() -> years.forEach(year -> {
            redisTemplate.opsForValue().setBit(BLOOM_FILTER_YEARS.getInfo(), year, true);
            blogService.getCountByYear(year);
        }), taskExecutor);

        // unlock user & del statistic & del hot read
        CompletableFuture.runAsync(() -> {
            List<Long> ids = userService.findIdsByStatus(StatusEnum.HIDE.getCode());
            ids.forEach(id -> {
                UserEntityVo user = userService.findById(id);
                user.setStatus(StatusEnum.NORMAL.getCode());
                var req = new UserEntityReq();
                req.setAvatar(user.getAvatar());
                req.setEmail(user.getEmail());
                req.setId(user.getId());
                req.setNickname(user.getNickname());
                req.setStatus(user.getStatus());
                req.setRole(user.getRole());
                req.setPhone(user.getPhone());
                req.setUsername(user.getUsername());
                userService.saveOrUpdate(req);
            });

            var now = LocalDateTime.now();

            int hourOfDay = now.getHour();
            int dayOfWeek = now.getDayOfWeek().getValue();
            int dayOfMonth = now.getDayOfMonth();
            int dayOfYear = now.getDayOfYear();

            if (hourOfDay == 0) {
                redisTemplate.delete(DAY_VISIT.getInfo());
                if (dayOfWeek == 1) {
                    redisTemplate.delete(WEEK_VISIT.getInfo());
                    redisTemplate.unlink(HOT_READ.getInfo());
                }
                if (dayOfMonth == 1) {
                    redisTemplate.delete(MONTH_VISIT.getInfo());
                }
                if (dayOfYear == 1) {
                    redisTemplate.delete(YEAR_VISIT.getInfo());
                }
            }
        }, taskExecutor);
    }
}
