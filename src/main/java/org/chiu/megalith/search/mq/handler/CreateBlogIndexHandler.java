package org.chiu.megalith.search.mq.handler;

import org.chiu.megalith.manage.entity.BlogEntity;
import org.chiu.megalith.infra.constant.BlogOperateEnum;
import org.chiu.megalith.manage.repository.BlogRepository;
import org.chiu.megalith.search.document.BlogDocument;

import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.time.ZoneId;
import java.time.ZonedDateTime;


@Component
public final class CreateBlogIndexHandler extends BlogIndexSupport {

    private final ElasticsearchTemplate elasticsearchTemplate;

    public CreateBlogIndexHandler(StringRedisTemplate redisTemplate,
                                  BlogRepository blogRepository,
                                  ElasticsearchTemplate elasticsearchTemplate) {
        super(redisTemplate, blogRepository);
        this.elasticsearchTemplate = elasticsearchTemplate;
    }

    @Override
    public boolean supports(BlogOperateEnum blogOperateEnum) {
        return BlogOperateEnum.CREATE.equals(blogOperateEnum);
    }

    @Override
    protected void elasticSearchProcess(BlogEntity blog) {
        var blogDocument = BlogDocument.builder()
                .id(blog.getId())
                .userId(blog.getUserId())
                .title(blog.getTitle())
                .description(blog.getDescription())
                .content(blog.getContent())
                .status(blog.getStatus()).link(blog.getLink())
                .created(ZonedDateTime.of(blog.getCreated(), ZoneId.of("Asia/Shanghai")))
                .updated(ZonedDateTime.of(blog.getUpdated(), ZoneId.of("Asia/Shanghai")))
                .build();

        elasticsearchTemplate.save(blogDocument);
    }

}
