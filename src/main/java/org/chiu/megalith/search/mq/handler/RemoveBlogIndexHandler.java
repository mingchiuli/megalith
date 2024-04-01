package org.chiu.megalith.search.mq.handler;

import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.repository.BlogRepository;
import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.blog.wrapper.BlogWrapper;
import org.chiu.megalith.infra.cache.CacheKeyGenerator;
import org.chiu.megalith.infra.search.BlogIndexEnum;
import org.chiu.megalith.search.document.BlogDocument;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;


import java.time.LocalDateTime;
import java.util.*;

import static org.chiu.megalith.infra.lang.Const.*;


/**
 * @author mingchiuli
 * @create 2022-12-03 3:55 pm
 */
@Component
public final class RemoveBlogIndexHandler extends BlogIndexSupport {
    private final ElasticsearchTemplate elasticsearchTemplate;

    private final BlogService blogService;

    public RemoveBlogIndexHandler(StringRedisTemplate redisTemplate,
                                  BlogRepository blogRepository,
                                  ElasticsearchTemplate elasticsearchTemplate,
                                  CacheKeyGenerator cacheKeyGenerator,
                                  BlogService blogService,
                                  RabbitTemplate rabbitTemplate) {
        super(redisTemplate, blogRepository, cacheKeyGenerator, rabbitTemplate);
        this.elasticsearchTemplate = elasticsearchTemplate;
        this.blogService = blogService;
    }

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

    @Override
    public boolean supports(BlogIndexEnum blogIndexEnum) {
        return BlogIndexEnum.REMOVE.equals(blogIndexEnum);
    }

    @Override
    protected Set<String> redisProcess(BlogEntity blog) {
        int year = blog.getCreated().getYear();
        Long id = blog.getId();
        //博客对象本身缓存
        String findById = cacheKeyGenerator.generateKey(BlogWrapper.class, "findById", new Class[]{Long.class}, new Object[]{id});
        String getCountByYear = cacheKeyGenerator.generateKey(BlogWrapper.class, "getCountByYear", new Class[]{Integer.class}, new Object[]{year});
        String status = cacheKeyGenerator.generateKey(BlogWrapper.class, "findStatusById", new Class[]{Long.class}, new Object[]{id});
        //删掉所有摘要缓存
        var start = LocalDateTime.of(year, 1, 1, 0, 0, 0);
        var end = LocalDateTime.of(year, 12, 31, 23, 59, 59);
        long count = blogRepository.count();
        long countYear = blogRepository.getPageCountYear(blog.getCreated(), start, end);
        Set<String> keys = cacheKeyGenerator.generateHotBlogsKeys(year, count, countYear);

        keys.add(READ_TOKEN.getInfo() + id);
        keys.add(findById);
        keys.add(getCountByYear);
        keys.add(status);
        //删除该年份的页面bloom，listPage的bloom，getCountByYear的bloom，后面逻辑重建
        keys.add(BLOOM_FILTER_YEAR_PAGE.getInfo() + blog.getCreated().getYear());
        keys.add(BLOOM_FILTER_PAGE.getInfo());
        keys.add(BLOOM_FILTER_YEARS.getInfo());
        //暂存区
        keys.add(TEMP_EDIT_BLOG.getInfo() + blog.getUserId() + ":" + id);
        //内容状态信息
        redisTemplate.delete(keys);
        keys.remove(BLOOM_FILTER_YEAR_PAGE.getInfo() + blog.getCreated().getYear());
        keys.remove(BLOOM_FILTER_PAGE.getInfo());
        keys.remove(BLOOM_FILTER_YEARS.getInfo());

        //设置getBlogDetail的bloom
        redisTemplate.opsForValue().setBit(BLOOM_FILTER_BLOG.getInfo(), blog.getId(), false);
        //重置该年份的页面bloom
        Integer countByPeriod = blogRepository.countByCreatedBetween(start, end);
        int totalPageByPeriod = countByPeriod % blogPageSize == 0 ? countByPeriod / blogPageSize : countByPeriod / blogPageSize + 1;
        for (int i = 1; i <= totalPageByPeriod; i++) {
            redisTemplate.opsForValue().setBit(BLOOM_FILTER_YEAR_PAGE.getInfo() + year, i, true);
        }

        //listPage的bloom
        int totalPage = (int) (count % blogPageSize == 0 ? count / blogPageSize : count / blogPageSize + 1);
        for (int i = 1; i <= totalPage; i++) {
            redisTemplate.opsForValue().setBit(BLOOM_FILTER_PAGE.getInfo(), i, true);
        }

        //getCountByYear的bloom
        List<Integer> years = blogService.getYears();
        years.forEach(y -> redisTemplate.opsForValue().setBit(BLOOM_FILTER_YEARS.getInfo(), y, true));
    
        //删除最近热度
        redisTemplate.opsForZSet().remove(HOT_READ.getInfo(), id.toString());
        
        return keys;
    }

    @Override
    protected void elasticSearchProcess(BlogEntity blog) {
        elasticsearchTemplate.delete(blog.getId().toString(), BlogDocument.class);
    }
}
