package org.chiu.megalith.blog.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.blog.convertor.BlogDescriptionVoConvertor;
import org.chiu.megalith.blog.convertor.BlogExhibitDtoConvertor;
import org.chiu.megalith.blog.dto.BlogExhibitDto;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.repository.BlogRepository;
import org.chiu.megalith.blog.vo.BlogDescriptionVo;
import org.chiu.megalith.infra.cache.Cache;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.repository.UserRepository;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.Objects;

import static org.chiu.megalith.infra.lang.ExceptionMessage.NO_FOUND;

@Component
@RequiredArgsConstructor
public class BlogWrapper {

    private final BlogRepository blogRepository;

    private final UserRepository userRepository;

    private final StringRedisTemplate redisTemplate;

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

    @Cache(prefix = Const.HOT_BLOG)
    public BlogExhibitDto findById(Long id) {
        BlogEntity blogEntity = blogRepository.findById(id)
                .orElseThrow(() -> new MissException(NO_FOUND));

        UserEntity user = userRepository.findById(blogEntity.getUserId())
                .orElseThrow(() -> new MissException(NO_FOUND));
        return BlogExhibitDtoConvertor.convert(blogEntity, user);
    }

    @Async("commonExecutor")
    public void setReadCount(Long id) {
        blogRepository.setReadCount(id);
        redisTemplate.opsForZSet().incrementScore(Const.HOT_READ.getInfo(), id.toString(), 1);
    }

    @Cache(prefix = Const.BLOG_STATUS)
    public Integer findStatusById(Long blogId) {
        return blogRepository.findStatusById(blogId);
    }

    @Cache(prefix = Const.HOT_BLOGS)
    public PageAdapter<BlogDescriptionVo> findPage(Integer currentPage, Integer year) {
        var pageRequest = PageRequest.of(currentPage - 1,
                blogPageSize,
                Sort.by("created").descending());

        Page<BlogEntity> page = Objects.equals(year, Integer.MIN_VALUE) ?
                blogRepository.findPage(pageRequest) :
                blogRepository.findPageByCreatedBetween(pageRequest, LocalDateTime.of(year, 1, 1, 0, 0, 0),
                LocalDateTime.of(year, 12, 31, 23, 59, 59));

        return BlogDescriptionVoConvertor.convert(page);
    }

    @Cache(prefix = Const.HOT_BLOG)
    public Long getCountByYear(Integer year) {
        return blogRepository.countByCreatedBetween(LocalDateTime.of(year, 1, 1, 0, 0, 0),
                LocalDateTime.of(year, 12, 31, 23, 59, 59));
    }

}
