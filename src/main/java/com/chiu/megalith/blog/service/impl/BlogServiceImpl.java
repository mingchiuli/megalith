package com.chiu.megalith.blog.service.impl;


import com.chiu.megalith.backstage.entity.UserEntity;
import com.chiu.megalith.backstage.service.UserService;
import com.chiu.megalith.blog.cache.Cache;
import com.chiu.megalith.blog.dto.BlogEntityDto;
import com.chiu.megalith.blog.entity.BlogEntity;
import com.chiu.megalith.blog.repository.BlogRepository;
import com.chiu.megalith.blog.service.BlogService;
import com.chiu.megalith.blog.vo.BlogEntityVo;
import com.chiu.megalith.common.config.RabbitConfig;
import com.chiu.megalith.common.exception.AuthenticationException;
import com.chiu.megalith.common.exception.NotFoundException;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.page.PageAdapter;
import com.chiu.megalith.common.search.BlogIndexEnum;
import com.chiu.megalith.common.search.BlogSearchIndexMessage;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.connection.CorrelationData;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.core.NestedRuntimeException;
import org.springframework.dao.DataAccessException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.SessionCallback;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.lang.NonNull;
import org.springframework.scheduling.annotation.Async;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.TimeUnit;

/**
 * @author mingchiuli
 * @create 2022-11-27 2:10 pm
 */
@Service
@Slf4j
@RequiredArgsConstructor
public class BlogServiceImpl implements BlogService {

    private final BlogRepository blogRepository;

    private final StringRedisTemplate redisTemplate;

    private final ObjectMapper objectMapper;

    private final UserService userService;

    private final RabbitTemplate rabbitTemplate;

    private final Integer blogPageSize = Integer.parseInt(Const.BLOG_PAGE_SIZE.getMsg());


    @Cache(prefix = Const.HOT_BLOG)
    public BlogEntity findByIdAndStatus(Long id, Integer status) {
        return blogRepository.findByIdAndStatus(id, status).orElseThrow();
    }

    @Async(value = "readCountThreadPoolExecutor")
    @Override
    @SuppressWarnings("unchecked")
    public void setReadCount(Long id) {
        blogRepository.setReadCount(id);
        try {
            redisTemplate.execute(new SessionCallback<>() {
                @Override
                public List<Object> execute(@NonNull RedisOperations operations) throws DataAccessException {
                    operations.multi();
                    operations.opsForValue().setIfAbsent(Const.READ_RECENT.getMsg() + id, 0, 7, TimeUnit.DAYS);
                    operations.opsForValue().increment(Const.READ_RECENT.getMsg() + id, 1);
                    return operations.exec();
                }
            });
        } catch (NestedRuntimeException e) {
            log.error(e.getMessage());
        }

    }

    @Override
    public BlogEntity findById(Long id) {
        return blogRepository.findById(id).orElseThrow(() -> new NotFoundException("blog not exist"));
    }

    @Override
    public PageAdapter<BlogEntity> listPage(Integer currentPage) {
        Pageable pageRequest = PageRequest.of(currentPage - 1,
                blogPageSize,
                Sort.by("created").descending());
        Page<BlogEntity> page = blogRepository.findAll(pageRequest);
        return new PageAdapter<>(page);
    }

    @Override
    public PageAdapter<BlogEntity> listPageByYear(Integer currentPage, Integer year) {
        LocalDateTime start = LocalDateTime.of(year, 1, 1 , 0, 0, 0);
        LocalDateTime end = LocalDateTime.of(year, 12, 31 , 23, 59, 59);
        Pageable pageRequest = PageRequest.of(currentPage - 1,
                blogPageSize,
                Sort.by("created").descending());
        Page<BlogEntity> page = blogRepository.findAllByYear(pageRequest, start, end);
        return new PageAdapter<>(page);
    }

    @Override
    public Integer getCountByYear(Integer year) {
        LocalDateTime start = LocalDateTime.of(year, 1, 1 , 0, 0, 0);
        LocalDateTime end = LocalDateTime.of(year, 12, 31 , 23, 59, 59);
        return blogRepository.countByYear(start, end);
    }

    @Override
    public BlogEntity getLockedBlog(Long blogId, String token) {
        token = token.trim();
        String password = redisTemplate.opsForValue().get(Const.READ_TOKEN);
        if (StringUtils.hasLength(token) && StringUtils.hasLength(password)) {
            if (password.equals(token)) {
                return blogRepository.findByIdAndStatus(blogId, 1).orElseThrow(() -> new NotFoundException("status error"));
            }
        }
        return null;
    }

    @Override
    public Integer findStatusById(Long blogId) {
        return blogRepository.findStatusById(blogId);
    }

    @Override
    public List<Integer> searchYears() {
        return blogRepository.searchYears();
    }


    @Override
    public List<BlogEntity> findAll() {
        return new ArrayList<>(blogRepository.findAll());
    }

    @Override
    public Long count() {
        return blogRepository.count();

    }

    @Override
    public void saveOrUpdate(BlogEntityVo blog) {
        Long userId = Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());
        var ref = new Object() {
            BlogEntity blogEntity;
            BlogIndexEnum type;
        };

        Optional.ofNullable(blog.getId()).ifPresentOrElse((id) -> {
            ref.blogEntity = blogRepository.findById(blog.getId()).orElseThrow(() -> new NotFoundException("blog not exist"));
            Assert.isTrue(ref.blogEntity.getUserId().equals(userId), "must edit your blog!");
            ref.type = BlogIndexEnum.UPDATE;
        }, () -> {
            ref.blogEntity = BlogEntity.
                    builder().
                    created(LocalDateTime.now()).
                    userId(userId).
                    readCount(0L).
                    build();
            ref.type = BlogIndexEnum.CREATE;
        });


        BeanUtils.copyProperties(blog, ref.blogEntity);
        blogRepository.save(ref.blogEntity);

        //通知消息给mq,更新并删除缓存
        CorrelationData correlationData = new CorrelationData();
        //防止重复消费
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR + correlationData.getId(),
                        ref.type.name() + "_" + ref.blogEntity.getId(),
                        10,
                        TimeUnit.SECONDS);

        rabbitTemplate.convertAndSend(
                RabbitConfig.ES_EXCHANGE,
                RabbitConfig.ES_BINDING_KEY,
                new BlogSearchIndexMessage(ref.blogEntity.getId(), ref.type, ref.blogEntity.getCreated().getYear()),
                correlationData);
    }


    @SneakyThrows
    @Override
    public void deleteBlogs(List<Long> ids) {
        Long userId = Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());

        for (Long id : ids) {
            Optional<BlogEntity> optionalBlog = blogRepository.findById(id);
            BlogEntity blogEntity = optionalBlog.orElseThrow(() -> new NotFoundException("blog not exist"));

            if (!blogEntity.getUserId().equals(userId)) {
                throw new AuthenticationException("must delete own blog");
            }

            blogRepository.delete(blogEntity);

            redisTemplate.opsForValue().set(userId + Const.QUERY_DELETED.getMsg() + id,
                    objectMapper.writeValueAsString(blogEntity),
                    7,
                    TimeUnit.DAYS);

            CorrelationData correlationData = new CorrelationData();
            //防止重复消费
            redisTemplate.opsForValue().set(Const.CONSUME_MONITOR + correlationData.getId(),
                    BlogIndexEnum.REMOVE.name() + "_" + id,
                    30,
                    TimeUnit.SECONDS);

            rabbitTemplate.convertAndSend(
                    RabbitConfig.ES_EXCHANGE,
                    RabbitConfig.ES_BINDING_KEY,
                    new BlogSearchIndexMessage(id, BlogIndexEnum.REMOVE, blogEntity.getCreated().getYear()), correlationData);
        }
    }

    @Override
    public void setBlogToken() {
        String token = UUID.randomUUID().toString();
        redisTemplate.opsForValue().set(Const.READ_TOKEN.getMsg(), token, 24, TimeUnit.HOURS);
    }

    @Override
    public String getBlogToken() {
        return Optional.ofNullable(redisTemplate.opsForValue().get(Const.READ_TOKEN)).
                orElse("read token is not exist");
    }

    @Override
    public PageAdapter<BlogEntityDto> getAllABlogs(Integer currentPage, Integer size) {
        Long userId = Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());

        Pageable pageRequest = PageRequest.of(currentPage - 1, size, Sort.by("created").descending());
        Page<BlogEntity> page = blogRepository.findAllAdmin(pageRequest, userId);
        ArrayList<BlogEntityDto> entities = new ArrayList<>();

        page.getContent().forEach(blogEntity -> {
            BlogEntityDto entityDto = new BlogEntityDto();
            BeanUtils.copyProperties(blogEntity, entityDto);
            Integer readNum = Integer.valueOf(Optional.ofNullable(redisTemplate.opsForValue().get(Const.READ_RECENT.getMsg() + blogEntity.getId())).orElse("0"));
            UserEntity userEntity = userService.findUsernameById(blogEntity.getUserId());
            entityDto.setUsername(userEntity.getUsername());
            entityDto.setReadRecent(readNum);
            entities.add(entityDto);
        });

        return PageAdapter.<BlogEntityDto>builder().
                content(entities).
                last(page.isLast()).
                first(page.isFirst()).
                pageNumber(page.getNumber()).
                totalPages(page.getTotalPages()).
                pageSize(page.getSize()).
                totalElements(page.getTotalElements()).
                empty(page.isEmpty()).
                build();
    }
}
