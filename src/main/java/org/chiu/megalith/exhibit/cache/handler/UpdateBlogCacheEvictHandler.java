package org.chiu.megalith.exhibit.cache.handler;

import lombok.SneakyThrows;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.exhibit.wrapper.BlogWrapper;
import org.chiu.megalith.infra.cache.CacheKeyGenerator;
import org.chiu.megalith.infra.constant.BlogOperateEnum;
import org.chiu.megalith.websocket.key.KeyFactory;
import org.chiu.megalith.blog.repository.BlogRepository;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.time.LocalDateTime;
import java.util.Set;

import static org.chiu.megalith.infra.lang.Const.READ_TOKEN;
import static org.chiu.megalith.infra.lang.StatusEnum.NORMAL;

@Component
public final class UpdateBlogCacheEvictHandler extends BlogCacheEvictHandler {


    private final CacheKeyGenerator cacheKeyGenerator;


    public UpdateBlogCacheEvictHandler(StringRedisTemplate redisTemplate,
                                       BlogRepository blogRepository,
                                       CacheKeyGenerator cacheKeyGenerator,
                                       RabbitTemplate rabbitTemplate) {
        super(redisTemplate, blogRepository, rabbitTemplate);
        this.cacheKeyGenerator = cacheKeyGenerator;
    }

    @Override
    public boolean supports(BlogOperateEnum blogOperateEnum) {
        return BlogOperateEnum.UPDATE.equals(blogOperateEnum);
    }

    @SneakyThrows
    @Override
    public Set<String> redisProcess(BlogEntity blogEntity) {
        Long id = blogEntity.getId();
        int year = blogEntity.getCreated().getYear();
        Integer status = blogEntity.getStatus();

        //不分年份的页数
        LocalDateTime start = LocalDateTime.of(year, 1, 1, 0, 0, 0);
        LocalDateTime end = LocalDateTime.of(year, 12, 31, 23, 59, 59);

        long countAfter = blogRepository.countByCreatedGreaterThanEqual(blogEntity.getCreated());
        long countYearAfter = blogRepository.getPageCountYear(blogEntity.getCreated(), start, end);
        Set<String> keys = cacheKeyGenerator.generateBlogKey(countAfter, countYearAfter, year);

        //博客对象本身缓存
        Method findByIdAndVisibleMethod = BlogWrapper.class.getMethod("findById", Long.class);
        String findByIdAndVisible = cacheKeyGenerator.generateKey(findByIdAndVisibleMethod, id);
        Method statusMethod = BlogWrapper.class.getMethod("findStatusById", Long.class);
        String statusKey = cacheKeyGenerator.generateKey(statusMethod, id);

        keys.add(findByIdAndVisible);
        keys.add(statusKey);
        if (NORMAL.getCode().equals(status)) {
            keys.add(READ_TOKEN.getInfo() + id);
        }

        String blogEditKey = KeyFactory.createBlogEditRedisKey(blogEntity.getUserId(), id);
        //暂存区
        keys.add(blogEditKey);
        //内容状态信息
        redisTemplate.delete(keys);
        if (NORMAL.getCode().equals(status)) {
            keys.remove(READ_TOKEN.getInfo() + id);
        }
        keys.remove(blogEditKey);

        return keys;
    }
}
