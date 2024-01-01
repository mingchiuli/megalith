package org.chiu.megalith.blog.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.repository.BlogRepository;
import org.chiu.megalith.blog.vo.BlogDescriptionVo;
import org.chiu.megalith.blog.vo.BlogExhibitVo;
import org.chiu.megalith.infra.cache.Cache;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.lang.StatusEnum;
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
    public BlogExhibitVo findById(Long id, Boolean visible) {
        BlogEntity blogEntity = Boolean.FALSE.equals(
                visible) ? blogRepository.findByIdAndStatus(id, StatusEnum.NORMAL.getCode())
                .orElseGet(BlogEntity::new)
                : blogRepository.findById(id)
                .orElseThrow(() -> new MissException(NO_FOUND));

        if (Objects.isNull(blogEntity.getId())) {
            return BlogExhibitVo.builder().build();
        }

        UserEntity user = userRepository.findById(blogEntity.getUserId())
                .orElseThrow(() -> new MissException(NO_FOUND));
        return BlogExhibitVo.builder()
                .title(blogEntity.getTitle())
                .description(blogEntity.getDescription())
                .content(blogEntity.getContent())
                .readCount(blogEntity.getReadCount())
                .nickname(user.getNickname())
                .avatar(user.getAvatar())
                .created(blogEntity.getCreated())
                .readCount(blogEntity.getReadCount())
                .build();
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

        Page<BlogEntity> page = Objects.equals(year, Integer.MIN_VALUE) ? blogRepository.findPage(pageRequest)
                : blogRepository.findPageByCreatedBetween(pageRequest, LocalDateTime.of(year, 1, 1, 0, 0, 0),
                LocalDateTime.of(year, 12, 31, 23, 59, 59));

        return new PageAdapter<>(page.map(blogEntity -> BlogDescriptionVo.builder()
                .id(blogEntity.getId())
                .description(blogEntity.getDescription())
                .title(blogEntity.getTitle())
                .created(blogEntity.getCreated())
                .link(blogEntity.getLink())
                .build()));
    }

    @Cache(prefix = Const.HOT_BLOG)
    public Integer getCountByYear(Integer year) {
        return blogRepository.countByCreatedBetween(LocalDateTime.of(year, 1, 1, 0, 0, 0),
                LocalDateTime.of(year, 12, 31, 23, 59, 59));
    }

}
