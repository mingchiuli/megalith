package org.chiu.megalith.exhibit.cache.handler;

import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.infra.cache.CacheKeyGenerator;
import org.chiu.megalith.infra.constant.BlogOperateEnum;
import org.chiu.megalith.websocket.key.KeyFactory;
import org.chiu.megalith.blog.repository.BlogRepository;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.Set;

import static org.chiu.megalith.infra.lang.Const.*;
import static org.chiu.megalith.infra.lang.Const.BLOOM_FILTER_YEARS;

@Component
public final class CreateBlogCacheEvictHandler extends BlogCacheEvictHandler {

    private final CacheKeyGenerator cacheKeyGenerator;

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

    public CreateBlogCacheEvictHandler(StringRedisTemplate redisTemplate,
                                       BlogRepository blogRepository,
                                       CacheKeyGenerator cacheKeyGenerator,
                                       RabbitTemplate rabbitTemplate) {
        super(redisTemplate, blogRepository, rabbitTemplate);
        this.cacheKeyGenerator = cacheKeyGenerator;
    }

    @Override
    public boolean supports(BlogOperateEnum blogOperateEnum) {
        return BlogOperateEnum.CREATE.equals(blogOperateEnum);
    }

    @Override
    public Set<String> redisProcess(BlogEntity blogEntity) {
        Long id = blogEntity.getId();
        int year = blogEntity.getCreated().getYear();
        //删除listPageByYear、listPage、getCountByYear所有缓存，该年份的页面bloom，编辑暂存区数据
        var start = LocalDateTime.of(year, 1, 1, 0, 0, 0);
        var end = LocalDateTime.of(year, 12, 31, 23, 59, 59);
        long count = blogRepository.count();
        long countYear = blogRepository.countByCreatedBetween(start, end);
        Set<String> keys = cacheKeyGenerator.generateHotBlogsKeys(year, count, countYear);
        String blogEditKey = KeyFactory.createBlogEditRedisKey(blogEntity.getUserId(), null);
        keys.add(blogEditKey);
        redisTemplate.delete(keys);
        keys.remove(blogEditKey);

        //重新构建该年份的页面bloom
        int totalPageByPeriod = (int) (countYear % blogPageSize == 0 ? countYear / blogPageSize : countYear / blogPageSize + 1);
        for (int i = 1; i <= totalPageByPeriod; i++) {
            redisTemplate.opsForValue().setBit(BLOOM_FILTER_YEAR_PAGE.getInfo() + year, i, true);
        }

        //listPage的bloom
        int totalPage = (int) (count % blogPageSize == 0 ? count / blogPageSize : count / blogPageSize + 1);
        for (int i = 1; i <= totalPage; i++) {
            redisTemplate.opsForValue().setBit(BLOOM_FILTER_PAGE.getInfo(), i, true);
        }

        //设置getBlogDetail的bloom, getBlogStatus的bloom(其实同一个bloom)和最近阅读数
        redisTemplate.opsForValue().setBit(BLOOM_FILTER_BLOG.getInfo(), id, true);

        //年份过滤bloom更新,getCountByYear的bloom
        redisTemplate.opsForValue().setBit(BLOOM_FILTER_YEARS.getInfo(), year, true);
        return keys;
    }
}
