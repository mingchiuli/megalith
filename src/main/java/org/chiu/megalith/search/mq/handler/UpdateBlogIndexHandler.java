package org.chiu.megalith.search.mq.handler;

import lombok.SneakyThrows;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.repository.BlogRepository;
import org.chiu.megalith.blog.wrapper.BlogWrapper;
import org.chiu.megalith.infra.cache.CacheKeyGenerator;
import org.chiu.megalith.infra.key.KeyFactory;
import org.chiu.megalith.infra.search.BlogIndexEnum;
import org.chiu.megalith.search.document.BlogDocument;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;


import java.lang.reflect.Method;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Set;

import static org.chiu.megalith.infra.lang.Const.*;
import static org.chiu.megalith.infra.lang.StatusEnum.NORMAL;


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
                                  CacheKeyGenerator cacheKeyGenerator,
                                  RabbitTemplate rabbitTemplate) {
        super(redisTemplate, blogRepository, cacheKeyGenerator, rabbitTemplate);
        this.elasticsearchTemplate = elasticsearchTemplate;
    }

    @Override
    public boolean supports(BlogIndexEnum blogIndexEnum) {
        return BlogIndexEnum.UPDATE.equals(blogIndexEnum);
    }

    @SneakyThrows
    @Override
    protected Set<String> redisProcess(BlogEntity blog) {
        Long id = blog.getId();
        int year = blog.getCreated().getYear();
        //不分年份的页数
        LocalDateTime start = LocalDateTime.of(year, 1, 1, 0, 0, 0);
        LocalDateTime end = LocalDateTime.of(year, 12, 31, 23, 59, 59);

        long countAfter = blogRepository.countByCreatedGreaterThanEqual(blog.getCreated());
        long countYearAfter = blogRepository.getPageCountYear(blog.getCreated(), start, end);
        Set<String> keys = cacheKeyGenerator.generateBlogKey(countAfter, countYearAfter, year);

        //博客对象本身缓存
        Method findByIdAndVisibleMethod = BlogWrapper.class.getMethod("findById", Long.class);
        String findByIdAndVisible = cacheKeyGenerator.generateKey(findByIdAndVisibleMethod, id);
        Method statusMethod = BlogWrapper.class.getMethod("findStatusById", Long.class);
        String status = cacheKeyGenerator.generateKey(statusMethod, id);

        keys.add(findByIdAndVisible);
        keys.add(status);
        if (NORMAL.getCode().equals(blog.getStatus())) {
            keys.add(READ_TOKEN.getInfo() + id);
        }

        String blogEditKey = KeyFactory.createBlogEditRedisKey(blog.getUserId(), id);
        //暂存区
        keys.add(blogEditKey);
        //内容状态信息
        redisTemplate.delete(keys);
        if (NORMAL.getCode().equals(blog.getStatus())) {
            keys.remove(READ_TOKEN.getInfo() + id);
        }
        keys.remove(blogEditKey);

        return keys;
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
                .updated(ZonedDateTime.of(blog.getUpdated(), ZoneId.of("Asia/Shanghai")))
                .build();

        elasticsearchTemplate.update(blogDocument);
    }
}
