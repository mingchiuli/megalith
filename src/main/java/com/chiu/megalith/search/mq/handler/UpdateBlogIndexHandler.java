package com.chiu.megalith.search.mq.handler;

import com.chiu.megalith.exhibit.controller.BlogController;
import com.chiu.megalith.exhibit.entity.BlogEntity;
import com.chiu.megalith.exhibit.repository.BlogRepository;
import com.chiu.megalith.exhibit.service.impl.BlogServiceImpl;
import com.chiu.megalith.infra.cache.CacheKeyGenerator;
import com.chiu.megalith.infra.search.BlogIndexEnum;
import com.chiu.megalith.search.document.BlogDocument;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.HashSet;

/**
 * @author mingchiuli
 * @create 2022-12-03 4:50 pm
 */
@Component
public final class UpdateBlogIndexHandler extends BlogIndexSupport {

    private final ElasticsearchTemplate elasticsearchTemplate;

    @Value("${blog.blog-page-size}")
    private Integer blogPageSize;

    public UpdateBlogIndexHandler(StringRedisTemplate redisTemplate,
                                  BlogRepository blogRepository,
                                  ElasticsearchTemplate elasticsearchTemplate,
                                  RedissonClient redisson,
                                  CacheKeyGenerator cacheKeyGenerator) {
        super(redisTemplate, blogRepository, redisson, cacheKeyGenerator);
        this.elasticsearchTemplate = elasticsearchTemplate;
    }

    @Override
    public boolean supports(BlogIndexEnum blogIndexEnum) {
        return BlogIndexEnum.UPDATE.equals(blogIndexEnum);
    }

    @Override
    protected void redisProcess(BlogEntity blog) {
        //不分年份的页数
        long count = blogRepository.countByCreatedAfter(blog.getCreated());
        count++;
        long pageNo = count % blogPageSize == 0 ? count / blogPageSize : count / blogPageSize + 1;
        String listPage = cacheKeyGenerator.generateKey(BlogController.class, "listPage", new Class[]{Integer.class}, new Object[]{pageNo});

        //分年份的页数
        long countYear = blogRepository.getPageCountYear(blog.getCreated(), blog.getCreated().getYear());
        countYear++;
        long pageYearNo = countYear % blogPageSize == 0 ? countYear / blogPageSize : countYear / blogPageSize + 1;
        String listPageByYear = cacheKeyGenerator.generateKey(BlogController.class, "listPageByYear", new Class[]{Integer.class, Integer.class}, new Object[]{pageYearNo, blog.getCreated().getYear()});

        //博客对象本身缓存
        String findByIdAndVisible = cacheKeyGenerator.generateKey(BlogServiceImpl.class, "findByIdAndVisible", new Class[]{Long.class}, new Object[]{blog.getId()});
        String getBlogStatus = cacheKeyGenerator.generateKey(BlogController.class, "getBlogStatus", new Class[]{Integer.class}, new Object[]{blog.getId()});
        String findTitleById = cacheKeyGenerator.generateKey(BlogServiceImpl.class, "findTitleById", new Class[]{Long.class}, new Object[]{blog.getId()});

        HashSet<String> keys = new HashSet<>(7);
        keys.add(findByIdAndVisible);
        keys.add(findTitleById);
        keys.add(getBlogStatus);
        keys.add(listPage);
        keys.add(listPageByYear);
        redisTemplate.unlink(keys);
    }

    @Override
    protected void elasticSearchProcess(BlogEntity blog) {
        BlogDocument blogDocument = BlogDocument
                .builder()
                .id(blog.getId())
                .userId(blog.getUserId())
                .title(blog.getTitle())
                .description(blog.getDescription()).content(blog.getContent())
                .status(blog.getStatus())
                .link(blog.getLink())
                .created(ZonedDateTime.of(blog.getCreated(), ZoneId.of("Asia/Shanghai")))
                .build();

        elasticsearchTemplate.save(blogDocument);
    }
}
