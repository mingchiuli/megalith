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

import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;

/**
 * @author mingchiuli
 * @create 2022-12-03 3:55 pm
 */
@Component
public final class RemoveBlogIndexHandler extends BlogIndexSupport {
    private final ElasticsearchTemplate elasticsearchTemplate;

    public RemoveBlogIndexHandler(StringRedisTemplate redisTemplate,
                                  BlogRepository blogRepository,
                                  ElasticsearchTemplate elasticsearchTemplate,
                                  RedissonClient redisson) {
        super(redisTemplate, blogRepository, redisson);
        this.elasticsearchTemplate = elasticsearchTemplate;
    }

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

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
        String contentKey = Const.HOT_BLOG.getInfo() + "::BlogServiceImpl::findByIdAndStatus" + builder;
        String statusKey = Const.BLOG_STATUS.getInfo() + "::BlogController::getBlogStatus" + builder;
        //年份缓存
        String yearsKey = Const.YEARS.getInfo() + "::BlogController::searchYears";
        String blogReadKey = Const.READ_RECENT.getInfo() + blog.getId();

        //删掉所有摘要缓存
        Set<String> keys = redisTemplate.keys(Const.HOT_BLOGS_PATTERN.getInfo());

        keys.add(yearsKey);
        keys.add(contentKey);
        keys.add(statusKey);
        keys.add(blogReadKey);
        //删除该年份的页面bloom，listPage的bloom，getCountByYear的bloom
        keys.add(Const.BLOOM_FILTER_YEAR_PAGE.getInfo() + blog.getCreated().getYear());
        keys.add(Const.BLOOM_FILTER_PAGE.getInfo());
        keys.add(Const.BLOOM_FILTER_YEARS.getInfo());
        redisTemplate.unlink(keys);

        int year = blog.getCreated().getYear();
        //设置getBlogDetail的bloom
        redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_BLOG.getInfo(), blog.getId(), false);
        //重置该年份的页面bloom
        LocalDateTime start = LocalDateTime.of(year, 1, 1 , 0, 0, 0);
        LocalDateTime end = LocalDateTime.of(year, 12, 31 , 23, 59, 59);
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

        //getCountByYear的bloom
        List<Integer> years = blogRepository.searchYears();
        years.forEach(y -> redisTemplate.opsForValue().setBit(Const.BLOOM_FILTER_YEARS.getInfo(), y, true));
    }

    @Override
    protected void elasticSearchProcess(BlogEntity blog) {
        elasticsearchTemplate.delete(blog.getId().toString(), BlogDocument.class);
    }
}
