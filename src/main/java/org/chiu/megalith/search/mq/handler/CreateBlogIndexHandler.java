package org.chiu.megalith.search.mq.handler;

import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.repository.BlogRepository;
import org.chiu.megalith.infra.cache.CacheKeyGenerator;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.search.BlogIndexEnum;
import org.chiu.megalith.search.document.BlogDocument;

import lombok.SneakyThrows;

import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Set;

@Component
public final class CreateBlogIndexHandler extends BlogIndexSupport {

    private final ElasticsearchTemplate elasticsearchTemplate;

    public CreateBlogIndexHandler(StringRedisTemplate redisTemplate,
                                  BlogRepository blogRepository,
                                  ElasticsearchTemplate elasticsearchTemplate,
                                  CacheKeyGenerator cacheKeyGenerator,
                                  RabbitTemplate rabbitTemplate) {
        super(redisTemplate, blogRepository, cacheKeyGenerator, rabbitTemplate);
        this.elasticsearchTemplate = elasticsearchTemplate;
    }

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

    @Override
    public boolean supports(BlogIndexEnum blogIndexEnum) {
        return BlogIndexEnum.CREATE.equals(blogIndexEnum);
    }

    @SneakyThrows
    @Override
    protected Set<String> redisProcess(BlogEntity blog) {
        //删除listPageByYear、listPage、getCountByYear所有缓存，该年份的页面bloom
        Set<String> keys = redisTemplate.keys(Const.HOT_BLOGS_PATTERN.getInfo());
        int year = blog.getCreated().getYear();
        keys.add(Const.BLOOM_FILTER_YEAR_PAGE.getInfo() + year);
        redisTemplate.unlink(keys);

        //重新构建该年份的页面bloom
        var start = LocalDateTime.of(year, 1, 1 , 0, 0, 0);
        var end = LocalDateTime.of(year, 12, 31 , 23, 59, 59);
        Integer countByPeriod = blogRepository.countByCreatedBetween(start, end);
        int totalPageByPeriod = countByPeriod % blogPageSize == 0 ? countByPeriod / blogPageSize : countByPeriod / blogPageSize + 1;
        for (int i = 1; i <= totalPageByPeriod; i++) {
            redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_YEAR_PAGE.getInfo() + year, i, true);
        }

        //listPage的bloom
        long count = blogRepository.count();
        int totalPage = (int) (count % blogPageSize == 0 ? count / blogPageSize : count / blogPageSize + 1);
        for (int i = 1; i <= totalPage; i++) {
            redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_PAGE.getInfo(), i, true);
        }

        //设置getBlogDetail的bloom, getBlogStatus的bloom(其实同一个bloom)和最近阅读数
        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_BLOG.getInfo(), blog.getId(), true);

        //年份过滤bloom更新,getCountByYear的bloom
        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_YEARS.getInfo(), year, true);
        return keys;
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
                .build();

        elasticsearchTemplate.save(blogDocument);
    }

}
