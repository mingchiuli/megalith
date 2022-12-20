package com.chiu.megalith.search.mq.handler.impl;

import com.chiu.megalith.blog.entity.BlogEntity;
import com.chiu.megalith.blog.repository.BlogRepository;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.search.BlogIndexEnum;
import com.chiu.megalith.search.document.BlogDocument;
import com.chiu.megalith.search.mq.handler.BlogIndexAbstractHandler;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.util.HashSet;
import java.util.Set;

/**
 * @author mingchiuli
 * @create 2022-12-03 3:55 pm
 */
@Component
public class RemoveBlogIndexHandler extends BlogIndexAbstractHandler {
    ElasticsearchTemplate elasticsearchTemplate;

    public RemoveBlogIndexHandler(StringRedisTemplate redisTemplate, BlogRepository blogRepository, ElasticsearchTemplate elasticsearchTemplate) {
        super(redisTemplate, blogRepository);
        this.elasticsearchTemplate = elasticsearchTemplate;
    }

    @Override
    public boolean supports(BlogIndexEnum blogIndexEnum) {
        return BlogIndexEnum.REMOVE.equals(blogIndexEnum);
    }

    @Override
    protected void redisProcess(BlogEntity blog) {
        //博客对象本身缓存
        StringBuilder builder = new StringBuilder();
        builder.append("::");
        builder.append(blog.getId());
        String contentKey = Const.HOT_BLOG + "::BlogServiceImpl::findByIdAndStatus" + builder;
        String statusKey = Const.BLOG_STATUS + "::BlogController::getBlogStatus" + builder;
        //年份缓存
        String yearsKey = Const.YEARS + "::BlogController::searchYears";
        String blogReadKey = Const.READ_RECENT.getMsg() + blog.getId();

        //删掉所有摘要缓存
        Set<String> keys = redisTemplate.keys(Const.HOT_BLOGS_PATTERN.getMsg());
        if (keys == null) {
            keys = new HashSet<>();
        }

        keys.add(yearsKey);
        keys.add(contentKey);
        keys.add(statusKey);
        keys.add(blogReadKey);
        keys.add(Const.BLOOM_FILTER_YEAR_PAGE.getMsg() + blog.getCreated().getYear());
        redisTemplate.unlink(keys);

        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_BLOG.getMsg(), blog.getId(), false);
        redisTemplate.delete(Const.READ_RECENT.getMsg() + blog.getId());
    }

    @Override
    protected void elasticSearchProcess(BlogEntity blog) {
        elasticsearchTemplate.delete(blog.getId().toString(), BlogDocument.class);
    }
}
