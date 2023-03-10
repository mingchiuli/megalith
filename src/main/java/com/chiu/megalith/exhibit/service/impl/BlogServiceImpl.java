package com.chiu.megalith.exhibit.service.impl;

import com.chiu.megalith.exhibit.cache.Cache;
import com.chiu.megalith.exhibit.dto.BlogEntityDto;
import com.chiu.megalith.exhibit.entity.BlogEntity;
import com.chiu.megalith.exhibit.repository.BlogRepository;
import com.chiu.megalith.exhibit.service.BlogService;
import com.chiu.megalith.manage.vo.BlogEntityVo;
import com.chiu.megalith.base.exception.AuthenticationExceptionImpl;
import com.chiu.megalith.base.exception.NotFoundException;
import com.chiu.megalith.base.lang.Const;
import com.chiu.megalith.base.page.PageAdapter;
import com.chiu.megalith.base.search.BlogIndexEnum;
import com.chiu.megalith.base.search.BlogSearchIndexMessage;
import com.chiu.megalith.base.utils.JsonUtils;
import com.chiu.megalith.search.config.ElasticSearchRabbitConfig;
import lombok.RequiredArgsConstructor;
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
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
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

    private final RabbitTemplate rabbitTemplate;

    private final JsonUtils jsonUtils;

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

    @Value("${blog.highest-role}")
    private String highestRole;

    public List<Long> findIdsByStatus(Integer status, Pageable pageRequest) {
        return blogRepository.findIdsByStatus(status, pageRequest);
    }

    @Cache(prefix = Const.HOT_BLOG)
    public BlogEntity findByIdAndStatus(Long id,
                                        Integer status) {
        return blogRepository.findByIdAndStatus(id, status).
                orElseThrow(() -> new NotFoundException("blog not found"));
    }

    @Async(value = "readCountThreadPoolExecutor")
    @Override
    public void setReadCount(Long id) {
        blogRepository.setReadCount(id);
        try {
            String prefix = Const.READ_RECENT.getInfo() + id;
            redisTemplate.execute(new SessionCallback<>() {
                @Override
                @SuppressWarnings("unchecked")
                public List<Object> execute(@NonNull RedisOperations operations) throws DataAccessException {
                    operations.multi();
                    operations.opsForValue().setIfAbsent(prefix, "0", 7, TimeUnit.DAYS);
                    operations.opsForValue().increment(prefix, 1);
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
    public PageAdapter<BlogEntity> listPageByYear(Integer currentPage,
                                                  Integer year) {
        LocalDateTime start = LocalDateTime.of(year, 1, 1 , 0, 0, 0);
        LocalDateTime end = LocalDateTime.of(year, 12, 31 , 23, 59, 59);
        Pageable pageRequest = PageRequest.of(currentPage - 1,
                blogPageSize,
                Sort.by("created").descending());
        Page<BlogEntity> page = blogRepository.findAllByCreatedBetween(pageRequest, start, end);
        return new PageAdapter<>(page);
    }

    @Override
    public Integer getCountByYear(Integer year) {
        LocalDateTime start = LocalDateTime.of(year, 1, 1 , 0, 0, 0);
        LocalDateTime end = LocalDateTime.of(year, 12, 31 , 23, 59, 59);
        return blogRepository.countByCreatedBetween(start, end);
    }

    @Override
    public BlogEntity getLockedBlog(Long blogId,
                                    String token) {
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
            ref.blogEntity = blogRepository.findById(blog.getId()).
                    orElseThrow(() -> new NotFoundException("blog not exist"));
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

        //???????????????mq,?????????????????????
        //??????????????????
        CorrelationData correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                ref.type + "_" + ref.blogEntity.getId(),
                30,
                TimeUnit.MINUTES);

        rabbitTemplate.convertAndSend(
                ElasticSearchRabbitConfig.ES_EXCHANGE,
                ElasticSearchRabbitConfig.ES_BINDING_KEY,
                new BlogSearchIndexMessage(ref.blogEntity.getId(), ref.type, ref.blogEntity.getCreated().getYear()),
                correlationData);
    }

    @Override
    public void deleteBlogs(List<Long> ids) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String authority = authentication.
                getAuthorities().
                stream().
                findFirst().
                map(GrantedAuthority::getAuthority).
                orElseThrow();
        long userId = Long.parseLong(authentication.getName());

        ids.forEach(id -> {
            BlogEntity blogEntity = blogRepository.findById(id).
                    orElseThrow(() -> new NotFoundException("blog not exist"));

            if (blogEntity.getUserId() != userId && !authority.equals(highestRole)) {
                throw new AuthenticationExceptionImpl("must delete own blog");
            }

            blogRepository.delete(blogEntity);

            redisTemplate.opsForValue().set(userId + Const.QUERY_DELETED.getInfo() + id,
                    jsonUtils.writeValueAsString(blogEntity),
                    7,
                    TimeUnit.DAYS);

            //??????????????????
            CorrelationData correlationData = new CorrelationData();
            redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                    BlogIndexEnum.REMOVE.name() + "_" + id,
                    30,
                    TimeUnit.MINUTES);

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
    public PageAdapter<BlogEntityDto> getAllABlogs(Integer currentPage,
                                                   Integer size) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        Long userId = Long.valueOf(authentication.getName());
        String authority = authentication.
                getAuthorities().
                stream().
                findFirst().
                map(GrantedAuthority::getAuthority).
                orElseThrow();

        Pageable pageRequest = PageRequest.of(currentPage - 1, size, Sort.by("created").descending());
        Page<BlogEntity> page;
        if (authority.equals(highestRole)) {
            page = blogRepository.findAll(pageRequest);
        } else {
            page = blogRepository.findAllByUserId(pageRequest, userId);
        }

        List<BlogEntityDto> entities = page.getContent().
                stream().
                map(blogEntity ->
                        BlogEntityDto.
                                builder().
                                id(blogEntity.getId()).
                                title(blogEntity.getTitle()).
                                description(blogEntity.getDescription()).
                                status(blogEntity.getStatus()).
                                created(blogEntity.getCreated()).
                                content(blogEntity.getContent()).
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
    public PageAdapter<BlogEntity> listDeletedBlogs(Integer currentPage,
                                                    Integer size) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        Set<String> keys = redisTemplate.keys(userId + Const.QUERY_DELETED.getInfo() + "*");

        int total = keys.size();
        int totalPages = total % size == 0 ? total / size : total / size + 1;

        List<String> list = redisTemplate.opsForValue().multiGet(keys);

        return PageAdapter.
                <BlogEntity>builder().
                content(list.
                        stream().
                        filter(Objects::nonNull).
                        map(str -> jsonUtils.readValue(str, BlogEntity.class)).
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
                build();
    }

    @Override
    public void recoverDeletedBlog(Long id) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        String blogStr = Optional.ofNullable(
                redisTemplate.opsForValue().get(userId + Const.QUERY_DELETED.getInfo() + id)
                ).
                orElseThrow(() -> new NotFoundException("blog is expired"));

        BlogEntity tempBlog = jsonUtils.readValue(blogStr, BlogEntity.class);
        BlogEntity blog = blogRepository.save(tempBlog);
        redisTemplate.delete(userId + Const.QUERY_DELETED.getInfo() + id);

        CorrelationData correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                BlogIndexEnum.CREATE.name() + "_" + id,
                30,
                TimeUnit.MINUTES);

        rabbitTemplate.convertAndSend(
                ElasticSearchRabbitConfig.ES_EXCHANGE,
                ElasticSearchRabbitConfig.ES_BINDING_KEY,
                new BlogSearchIndexMessage(blog.getId(), BlogIndexEnum.CREATE, blog.getCreated().getYear()),
                correlationData);
    }


    @Override
    public void changeBlogStatus(Long id,
                                 Integer status,
                                 Integer year) {
        blogRepository.setStatus(id, status);

        CorrelationData correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                BlogIndexEnum.UPDATE.name() + "_" + id,
                30,
                TimeUnit.MINUTES);

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
