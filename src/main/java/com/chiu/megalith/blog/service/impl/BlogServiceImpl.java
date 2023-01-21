package com.chiu.megalith.blog.service.impl;

import com.chiu.megalith.blog.cache.Cached;
import com.chiu.megalith.blog.dto.BlogEntityDto;
import com.chiu.megalith.blog.entity.BlogEntity;
import com.chiu.megalith.blog.repository.BlogRepository;
import com.chiu.megalith.blog.service.BlogService;
import com.chiu.megalith.blog.vo.BlogEntityVo;
import com.chiu.megalith.common.exception.AuthenticationExceptionImpl;
import com.chiu.megalith.common.exception.NotFoundException;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.page.PageAdapter;
import com.chiu.megalith.common.search.BlogIndexEnum;
import com.chiu.megalith.common.search.BlogSearchIndexMessage;
import com.chiu.megalith.common.utils.RedisUtils;
import com.chiu.megalith.search.config.ElasticSearchRabbitConfig;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.connection.CorrelationData;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
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

    private final RabbitTemplate rabbitTemplate;

    private final RedisUtils redisUtils;

    @Value("${blog.blog-page-size}")
    private int blogPageSize;


    @Cached(prefix = Const.HOT_BLOG)
    public BlogEntity findByIdAndStatus(Long id, Integer status) {
        return blogRepository.findByIdAndStatus(id, status).
                orElseThrow(() -> new NotFoundException("blog not found"));
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
                    operations.opsForValue().setIfAbsent(Const.READ_RECENT.getInfo() + id, "0", 7, TimeUnit.DAYS);
                    operations.opsForValue().increment(Const.READ_RECENT.getInfo() + id, 1);
                    return operations.exec();
                }
            });
        } catch (NestedRuntimeException e) {
            log.error(e.getMessage());
        }

    }

    @Override
    public BlogEntity findById(Long id) {
        return blogRepository.findById(id).
                orElseThrow(() -> new NotFoundException("blog not exist"));
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
                return blogRepository.findByIdAndStatus(blogId, 1).
                        orElseThrow(() -> new NotFoundException("status error"));
            }
        }
        throw new AuthenticationExceptionImpl("authorization exception");
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
        return blogRepository.findAll();
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

        Optional.ofNullable(blog.getId()).ifPresentOrElse(id -> {
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
        //防止重复消费
        CorrelationData correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                        ref.type.name() + "_" + ref.blogEntity.getId(),
                        30,
                        TimeUnit.SECONDS);

        rabbitTemplate.convertAndSend(
                ElasticSearchRabbitConfig.ES_EXCHANGE,
                ElasticSearchRabbitConfig.ES_BINDING_KEY,
                new BlogSearchIndexMessage(ref.blogEntity.getId(), ref.type, ref.blogEntity.getCreated().getYear()),
                correlationData);
    }

    @Override
    public void deleteBlogs(List<Long> ids) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        ids.forEach(id -> {
            BlogEntity blogEntity = blogRepository.findById(id).
                    orElseThrow(() -> new NotFoundException("blog not exist"));

            Assert.isTrue(blogEntity.getUserId() == userId, "must delete own blog");

            blogRepository.delete(blogEntity);

            redisTemplate.opsForValue().set(userId + Const.QUERY_DELETED.getInfo() + id,
                    redisUtils.writeValueAsString(blogEntity),
                    7,
                    TimeUnit.DAYS);

            //防止重复消费
            CorrelationData correlationData = new CorrelationData();
            redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                    BlogIndexEnum.REMOVE.name() + "_" + id,
                    30,
                    TimeUnit.SECONDS);

            rabbitTemplate.convertAndSend(
                    ElasticSearchRabbitConfig.ES_EXCHANGE,
                    ElasticSearchRabbitConfig.ES_BINDING_KEY,
                    new BlogSearchIndexMessage(id, BlogIndexEnum.REMOVE, blogEntity.getCreated().getYear()), correlationData);
        });
    }

    @Override
    public void setBlogToken() {
        String token = UUID.randomUUID().toString();
        redisTemplate.opsForValue().set(Const.READ_TOKEN.getInfo(), token, 24, TimeUnit.HOURS);
    }

    @Override
    public String getBlogToken() {
        return Optional.ofNullable(
                redisTemplate.opsForValue().get(Const.READ_TOKEN)
                ).
                orElse("read token is not exist");
    }

    @Override
    public PageAdapter<BlogEntityDto> getAllABlogs(Integer currentPage, Integer size) {
        Long userId = Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());

        Pageable pageRequest = PageRequest.of(currentPage - 1, size, Sort.by("created").descending());
        Page<BlogEntity> page = blogRepository.findAllAdmin(pageRequest, userId);

        List<BlogEntityDto> entities = page.getContent().
                stream().
                map(blogEntity ->
                        BlogEntityDto.builder().
                                id(blogEntity.
                                        getId()).
                                title(blogEntity.
                                        getTitle()).
                                description(blogEntity.
                                        getDescription()).
                                status(blogEntity.
                                        getStatus()).
                                created(blogEntity.
                                        getCreated()).
                                content(blogEntity.
                                        getContent()).
                                readRecent(Integer.valueOf(
                                        Optional.ofNullable(
                                                redisTemplate.opsForValue().get(Const.READ_RECENT.getInfo() + blogEntity.getId())
                                                ).
                                                orElse("0")
                                        )
                                ).
                                build()).
                toList();

        return PageAdapter.
                <BlogEntityDto>builder().
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

    @Override
    @SuppressWarnings("unchecked")
    public PageAdapter<BlogEntity> listDeletedBlogs(Integer currentPage, Integer size) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        Set<String> set = redisTemplate.keys(userId + Const.QUERY_DELETED.getInfo() + "*");
        var ref = new Object() {
            PageAdapter<BlogEntity> pageAdapter;
        };

        Optional.ofNullable(set).ifPresentOrElse(keys -> {
            int total = keys.size();
            int totalPages = total % size == 0 ? total / size : total / size + 1;

            List<String> stringList = redisTemplate.opsForValue().multiGet(keys);
            Optional.ofNullable(stringList).ifPresentOrElse(list ->
                    ref.pageAdapter = PageAdapter.
                            <BlogEntity>builder().
                            content(list.
                                    stream().
                                    map(str -> redisUtils.readValue(str, BlogEntity.class)).
                                    sorted((o1, o2) -> o2.getCreated().compareTo(o1.getCreated())).
                                    limit((long) currentPage * size).skip((long) (currentPage - 1) * size).
                                    toList()).
                            last(currentPage == totalPages).
                            first(currentPage == 1).
                            pageNumber(currentPage).
                            totalPages(totalPages).
                            pageSize(size).
                            totalElements(total).
                            empty(total == 0).
                            build(), () -> ref.pageAdapter = PageAdapter.emptyPage(blogPageSize));
        }, () -> ref.pageAdapter = PageAdapter.emptyPage(blogPageSize));

        return ref.pageAdapter;
    }

    @SneakyThrows
    @Override
    public void recoverDeletedBlog(Long id) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        String blogStr = Optional.ofNullable(
                redisTemplate.opsForValue().get(userId + Const.QUERY_DELETED.getInfo() + id)
                ).
                orElseThrow(() -> new NotFoundException("blog is expired"));

        BlogEntity tempBlog = objectMapper.readValue(blogStr, BlogEntity.class);
        BlogEntity blog = blogRepository.save(tempBlog);
        redisTemplate.delete(userId + Const.QUERY_DELETED.getInfo() + id);

        CorrelationData correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                BlogIndexEnum.CREATE.name() + "_" + blog.getId(),
                30,
                TimeUnit.SECONDS);

        rabbitTemplate.convertAndSend(
                ElasticSearchRabbitConfig.ES_EXCHANGE,
                ElasticSearchRabbitConfig.ES_BINDING_KEY,
                new BlogSearchIndexMessage(blog.getId(), BlogIndexEnum.CREATE, blog.getCreated().getYear()),
                correlationData);
    }


    @Override
    public void changeBlogStatus(Long id, Integer status, Integer year) {
        blogRepository.setStatus(id, status);

        CorrelationData correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                BlogIndexEnum.UPDATE + "_" + id,
                30,
                TimeUnit.SECONDS);

        rabbitTemplate.convertAndSend(
                ElasticSearchRabbitConfig.ES_EXCHANGE,
                ElasticSearchRabbitConfig.ES_BINDING_KEY,
                new BlogSearchIndexMessage(id, BlogIndexEnum.UPDATE, year),
                correlationData);
    }

    @Override
    public boolean exist(Long blogId) {
        return blogRepository.existsById(blogId);
    }
}
