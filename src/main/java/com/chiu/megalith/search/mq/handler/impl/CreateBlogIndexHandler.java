package com.chiu.megalith.search.mq.handler.impl;

import com.chiu.megalith.blog.entity.BlogEntity;
import com.chiu.megalith.blog.repository.BlogRepository;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.search.BlogIndexEnum;
import com.chiu.megalith.search.document.BlogDocument;
import com.chiu.megalith.search.mq.handler.BlogIndexAbstractHandler;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;

@Component
@Slf4j
public class CreateBlogIndexHandler extends BlogIndexAbstractHandler {
    private final ObjectMapper objectMapper;
    private final ElasticsearchTemplate elasticsearchTemplate;

    public CreateBlogIndexHandler(StringRedisTemplate redisTemplate, BlogRepository blogRepository, ObjectMapper objectMapper, ElasticsearchTemplate elasticsearchTemplate) {
        super(redisTemplate, blogRepository);
        this.objectMapper = objectMapper;
        this.elasticsearchTemplate = elasticsearchTemplate;
    }

    @Override
    public boolean supports(BlogIndexEnum blogIndexEnum) {
        return BlogIndexEnum.CREATE.equals(blogIndexEnum);
    }


    @SneakyThrows
    @Override
    protected void redisProcess(BlogEntity blog) {
        Set<String> keys = redisTemplate.keys(Const.HOT_BLOGS_PATTERN.getMsg());

        if (keys == null) {
            keys = new HashSet<>();
        }
        keys.add(Const.BLOOM_FILTER_YEAR_PAGE.getMsg() + blog.getCreated().getYear());
        redisTemplate.unlink(keys);

        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_BLOG.getMsg(), blog.getId(), true);
        redisTemplate.opsForValue().set(Const.READ_RECENT.getMsg() + blog.getId(), objectMapper.writeValueAsString(0), 7, TimeUnit.DAYS);

        //年份过滤bloom更新
        int year = blog.getCreated().getYear();
        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_YEARS.getMsg(), year, true);
    }

    @Override
    protected void elasticSearchProcess(BlogEntity blog) {
        BlogDocument blogDocument = BlogDocument.builder().
                id(blog.getId()).
                userId(blog.getUserId()).
                title(blog.getTitle()).
                description(blog.getDescription()).
                content(blog.getContent()).
                status(blog.getStatus()).
                link(blog.getLink()).
                created(ZonedDateTime.of(blog.getCreated(), ZoneId.of("Asia/Shanghai"))).
                build();

        elasticsearchTemplate.save(blogDocument);
    }

}
