package com.chiu.megalith.search.mq.handler;

import com.chiu.megalith.blog.controller.BlogController;
import com.chiu.megalith.blog.entity.BlogEntity;
import com.chiu.megalith.blog.repository.BlogRepository;
import com.chiu.megalith.blog.service.impl.BlogServiceImpl;
import com.chiu.megalith.infra.cache.CacheKeyGenerator;
import com.chiu.megalith.infra.search.BlogIndexEnum;
import com.chiu.megalith.search.document.BlogDocument;
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
                                  CacheKeyGenerator cacheKeyGenerator) {
        super(redisTemplate, blogRepository, cacheKeyGenerator);
        this.elasticsearchTemplate = elasticsearchTemplate;
    }

    @Override
    public boolean supports(BlogIndexEnum blogIndexEnum) {
        return BlogIndexEnum.UPDATE.equals(blogIndexEnum);
    }

    @Override
    protected void redisProcess(BlogEntity blog) {
        Long id = blog.getId();
        int year = blog.getCreated().getYear();
        //不分年份的页数
        long count = blogRepository.countByCreatedAfter(blog.getCreated());
        count++;
        long pageNo = count % blogPageSize == 0 ? count / blogPageSize : count / blogPageSize + 1;
        String findPage = cacheKeyGenerator.generateKey(BlogServiceImpl.class, "findPage", new Class[]{Integer.class, Integer.class}, new Object[]{pageNo, Integer.MIN_VALUE});

        //分年份的页数
        long countYear = blogRepository.getPageCountYear(blog.getCreated(), blog.getCreated().getYear());
        countYear++;
        long pageYearNo = countYear % blogPageSize == 0 ? countYear / blogPageSize : countYear / blogPageSize + 1;
        String findPageByYear = cacheKeyGenerator.generateKey(BlogServiceImpl.class, "findPage", new Class[]{Integer.class, Integer.class}, new Object[]{pageYearNo, year});

        //博客对象本身缓存
        String findByIdAndVisible = cacheKeyGenerator.generateKey(BlogServiceImpl.class, "findById", new Class[]{Long.class, Boolean.class}, new Object[]{id, false});
        String findByIdAndInvisible = cacheKeyGenerator.generateKey(BlogServiceImpl.class, "findById", new Class[]{Long.class, Boolean.class}, new Object[]{id, true});
        String getBlogStatus = cacheKeyGenerator.generateKey(BlogController.class, "getBlogStatus", new Class[]{Long.class}, new Object[]{id});

        HashSet<String> keys = new HashSet<>(7);
        keys.add(findByIdAndVisible);
        keys.add(findByIdAndInvisible);
        keys.add(getBlogStatus);
        keys.add(findPage);
        keys.add(findPageByYear);
        redisTemplate.unlink(keys);
    }

    @Override
    protected void elasticSearchProcess(BlogEntity blog) {
        var blogDocument = BlogDocument.builder()
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
