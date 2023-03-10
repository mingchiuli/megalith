package com.chiu.megalith.search.mq.handler;

import com.chiu.megalith.exhibit.entity.BlogEntity;
import com.chiu.megalith.exhibit.repository.BlogRepository;
import com.chiu.megalith.base.lang.Const;
import com.chiu.megalith.base.search.BlogIndexEnum;
import com.chiu.megalith.search.document.BlogDocument;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Set;

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
                                  RedissonClient redisson) {
        super(redisTemplate, blogRepository, redisson);
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
        String sb = "::" + pageNo;
        String pageNoPrefix = Const.HOT_BLOGS.getInfo() + "::BlogController::listPage" + sb;

        //分年份的页数
        long countYear = blogRepository.getPageCountYear(blog.getCreated(), blog.getCreated().getYear());
        countYear++;
        long pageYearNo = countYear % blogPageSize == 0 ? countYear / blogPageSize : countYear / blogPageSize + 1;
        String s = "::" + pageYearNo + "::" + blog.getCreated().getYear();
        String pageYearNoPrefix = Const.HOT_BLOGS.getInfo() + "::BlogController::listPageByYear" + s;

        //博客对象本身缓存
        StringBuilder builder = new StringBuilder();
        builder.append("::");
        builder.append(blog.getId());
        String contentKey = Const.HOT_BLOG.getInfo() + "::BlogServiceImpl::findByIdAndStatus" + builder;
        String statusKey = Const.BLOG_STATUS.getInfo() + "::BlogController::getBlogStatus" + builder;

        Set<String> keys = redisTemplate.keys(Const.HOT_BLOGS_PATTERN.getInfo());

        keys.add(contentKey);
        keys.add(statusKey);
        keys.add(pageNoPrefix);
        keys.add(pageYearNoPrefix);
        redisTemplate.unlink(keys);
    }

    @Override
    protected void elasticSearchProcess(BlogEntity blog) {
        BlogDocument blogDocument = BlogDocument.
                builder().
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
