package com.chiu.megalith.blog.service.impl;

import com.chiu.megalith.blog.vo.*;
import com.chiu.megalith.infra.utils.LuaScriptUtils;
import com.chiu.megalith.infra.cache.Cache;
import com.chiu.megalith.blog.dto.BlogEntityDto;
import com.chiu.megalith.blog.entity.BlogEntity;
import com.chiu.megalith.blog.repository.BlogRepository;
import com.chiu.megalith.blog.service.BlogService;
import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.service.UserService;
import com.chiu.megalith.infra.exception.AuthenticationExceptionImpl;
import com.chiu.megalith.infra.exception.NotFoundException;
import com.chiu.megalith.infra.lang.Const;
import com.chiu.megalith.infra.page.PageAdapter;
import com.chiu.megalith.infra.search.BlogIndexEnum;
import com.chiu.megalith.infra.search.BlogSearchIndexMessage;
import com.chiu.megalith.infra.utils.JsonUtils;
import com.chiu.megalith.search.config.ElasticSearchRabbitConfig;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.connection.CorrelationData;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.ZSetOperations;
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

    private final UserService userService;

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

    @Value("${blog.highest-role}")
    private String highestRole;

    public List<Long> findIds(Pageable pageRequest) {
        return blogRepository.findIds(pageRequest);
    }

    @Cache(prefix = Const.HOT_BLOG)
    @Override
    public BlogExhibitVo findById(Long id, Boolean visible) {
        BlogEntity blogEntity;
        if (visible) {
            blogEntity = blogRepository.findByIdAndStatus(id, 0)
                    .orElseThrow(() -> new NotFoundException("blog not found"));
        } else {
            blogEntity = blogRepository.findById(id)
                    .orElseThrow(() -> new NotFoundException("blog not exist"));
        }
        UserEntity user = userService.findById(blogEntity.getUserId());

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

    @Async("readCountThreadPoolExecutor")
    @Override
    public void setReadCount(Long id) {
        blogRepository.setReadCount(id);
        redisTemplate.opsForZSet().incrementScore(Const.HOT_READ.getInfo(), id.toString(), 1);
    }

    @Override
    public BlogEntity findById(Long id) {
        return blogRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("blog not exist"));
    }

    @Override
    @Cache(prefix = Const.HOT_BLOGS)
    public PageAdapter<BlogDescriptionVo> findPage(Integer currentPage,
                                                   Integer year) {
        Page<BlogEntity> page;

        Pageable pageRequest = PageRequest.of(currentPage - 1,
                blogPageSize,
                Sort.by("created").descending());

        if (Objects.equals(year, Integer.MIN_VALUE)) {
            page = blogRepository.findPage(pageRequest);
        } else {
            LocalDateTime start = LocalDateTime.of(year, 1, 1 , 0, 0, 0);
            LocalDateTime end = LocalDateTime.of(year, 12, 31 , 23, 59, 59);
            page = blogRepository.findPageByCreatedBetween(pageRequest, start, end);
        }

        return new PageAdapter<>(page.map(blogEntity ->
                BlogDescriptionVo.builder()
                        .id(blogEntity.getId())
                        .description(blogEntity.getDescription())
                        .title(blogEntity.getTitle())
                        .created(blogEntity.getCreated())
                        .link(blogEntity.getLink())
                        .build()));
    }

    @Override
    @Cache(prefix = Const.HOT_BLOG)
    public Integer getCountByYear(Integer year) {
        LocalDateTime start = LocalDateTime.of(year, 1, 1 , 0, 0, 0);
        LocalDateTime end = LocalDateTime.of(year, 12, 31 , 23, 59, 59);
        return blogRepository.countByCreatedBetween(start, end);
    }

    @Override
    public boolean checkToken(Long blogId,
                              String token) {
        token = token.trim();
        String password = redisTemplate.opsForValue().get(Const.READ_TOKEN.getInfo());
        if (StringUtils.hasLength(token) && StringUtils.hasLength(password)) {
            return password.equals(token);
        }
        return false;
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
        Long blogId = blog.getId();

        BlogEntity blogEntity;
        BlogIndexEnum type;

        if (Objects.nonNull(blogId)) {
            blogEntity = blogRepository.findById(blog.getId())
                    .orElseThrow(() -> new NotFoundException("blog not exist"));
            Assert.isTrue(Objects.equals(blogEntity.getUserId(), userId), "must edit your blog!");
            type = BlogIndexEnum.UPDATE;
        } else {
            blogEntity = BlogEntity.builder()
                    .created(LocalDateTime.now())
                    .userId(userId)
                    .readCount(0L)
                    .build();
            type = BlogIndexEnum.CREATE;
        }

        BeanUtils.copyProperties(blog, blogEntity);
        blogRepository.save(blogEntity);

        //通知消息给mq,更新并删除缓存
        //防止重复消费
        var correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                type + "_" + blogEntity.getId(),
                30,
                TimeUnit.MINUTES);

        rabbitTemplate.convertAndSend(
                ElasticSearchRabbitConfig.ES_EXCHANGE,
                ElasticSearchRabbitConfig.ES_BINDING_KEY,
                new BlogSearchIndexMessage(blogEntity.getId(), type, blogEntity.getCreated().getYear()),
                correlationData);
    }

    @Override
    public void deleteBlogs(List<Long> ids) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String authority = authentication.getAuthorities().stream()
                .findFirst()
                .map(GrantedAuthority::getAuthority)
                .orElseThrow();
        long userId = Long.parseLong(authentication.getName());

        ids.forEach(id -> {
            BlogEntity blogEntity = blogRepository.findById(id)
                    .orElseThrow(() -> new NotFoundException("blog not exist"));

            if (!Objects.equals(blogEntity.getUserId(), userId) && !Objects.equals(authority, highestRole)) {
                throw new AuthenticationExceptionImpl("must delete own blog");
            }

            blogRepository.delete(blogEntity);

            LocalDateTime now = LocalDateTime.now();
            blogEntity.setCreated(now);

            redisTemplate.execute(LuaScriptUtils.setBlogDeleteLua,
                    Collections.singletonList(Const.QUERY_DELETED.getInfo() + userId),
                    jsonUtils.writeValueAsString(blogEntity), "604800");

            //防止重复消费
            var correlationData = new CorrelationData();
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
    public String setBlogToken() {
        String token = UUID.randomUUID().toString();
        redisTemplate.opsForValue().set(Const.READ_TOKEN.getInfo(), token, 24, TimeUnit.HOURS);
        return token;
    }

    @Override
    public PageAdapter<BlogEntityDto> findAllABlogs(Integer currentPage,
                                                    Integer size) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        Long userId = Long.valueOf(authentication.getName());
        String authority = authentication.getAuthorities().stream()
                .findFirst()
                .map(GrantedAuthority::getAuthority)
                .orElseThrow();

        Pageable pageRequest = PageRequest.of(currentPage - 1, size, Sort.by("created").descending());
        Page<BlogEntity> page;
        if (authority.equals(highestRole)) {
            page = blogRepository.findAll(pageRequest);
        } else {
            page = blogRepository.findAllByUserId(pageRequest, userId);
        }

        List<BlogEntityDto> entities = page.getContent()
                .stream()
                .map(blogEntity ->
                        BlogEntityDto.builder()
                                .id(blogEntity.getId())
                                .title(blogEntity.getTitle())
                                .description(blogEntity.getDescription())
                                .readCount(blogEntity.getReadCount())
                                .recentReadCount(
                                        Optional.ofNullable(redisTemplate.opsForZSet().score(Const.HOT_READ.getInfo(), blogEntity.getId().toString()))
                                                .orElse(0.0))
                                .status(blogEntity.getStatus())
                                .created(blogEntity.getCreated())
                                .content(blogEntity.getContent())
                                .build())
                .toList();

        return PageAdapter.<BlogEntityDto>builder()
                .content(entities)
                .last(page.isLast())
                .first(page.isFirst())
                .pageNumber(page.getNumber())
                .totalPages(page.getTotalPages())
                .pageSize(page.getSize())
                .totalElements(page.getTotalElements())
                .empty(page.isEmpty())
                .build();
    }

    @Override
    public PageAdapter<BlogEntity> findDeletedBlogs(Integer currentPage,
                                                    Integer size) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        LocalDateTime now = LocalDateTime.now();
        List<BlogEntity> deletedBlogs = redisTemplate.opsForList().range(Const.QUERY_DELETED.getInfo() + userId, 0, -1).stream()
                .map(blogStr -> jsonUtils.readValue(blogStr, BlogEntity.class))
                .toList();

        int l = 0;
        for (BlogEntity blog : deletedBlogs) {
            if (now.minusDays(7).isAfter(blog.getCreated())) {
                l++;
            } else {
                break;
            }
        }

        int start = (currentPage - 1) * size;

        Long total = redisTemplate.execute(LuaScriptUtils.flushDelete,
                Collections.singletonList(Const.QUERY_DELETED.getInfo() + userId),
                String.valueOf(l), "-1");

        int totalPages = (int) (total % size == 0 ? total / size : total / size + 1);

        return PageAdapter.<BlogEntity>builder()
                .content(redisTemplate.opsForList().range(Const.QUERY_DELETED.getInfo() + userId, start, start + size).stream()
                        .map(str -> jsonUtils.readValue(str, BlogEntity.class))
                        .toList())
                .last(currentPage == totalPages)
                .first(currentPage == 1)
                .pageNumber(currentPage)
                .totalPages(totalPages)
                .pageSize(size)
                .totalElements(total)
                .empty(total == 0)
                .build();
    }

    @Override
    public void recoverDeletedBlog(Long id,
                                   Integer idx) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        String str = redisTemplate.opsForList().index(Const.QUERY_DELETED.getInfo() + userId, idx);

        BlogEntity tempBlog = jsonUtils.readValue(str, BlogEntity.class);
        BlogEntity blog = blogRepository.save(tempBlog);
        redisTemplate.opsForList().remove(Const.QUERY_DELETED.getInfo() + userId, 1, str);

        var correlationData = new CorrelationData();
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
                                 Integer status) {
        int year = blogRepository.findById(id).orElseThrow().getCreated().getYear();
        blogRepository.setStatus(id, status);

        var correlationData = new CorrelationData();
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

    @Override
    public VisitStatisticsVo getVisitStatistics() {
        List<Long> list = redisTemplate.execute(LuaScriptUtils.getVisitLua,
                List.of(Const.DAY_VISIT.getInfo(), Const.WEEK_VISIT.getInfo(), Const.MONTH_VISIT.getInfo(), Const.YEAR_VISIT.getInfo()));

        return VisitStatisticsVo.builder()
                .daySize(list.get(0))
                .weekSize(list.get(1))
                .monthSize(list.get(2))
                .yearSize(list.get(3))
                .build();
    }

    @Override
    public List<BlogHotReadVo> getScoreBlogs() {
        Set<ZSetOperations.TypedTuple<String>> set = redisTemplate.opsForZSet().reverseRangeWithScores(Const.HOT_READ.getInfo(), 0, 4);
        return set.stream()
                .map(item -> BlogHotReadVo.builder()
                        .id(Long.valueOf(item.getValue()))
                        .readCount(item.getScore().longValue())
                        .build())
                .toList();
    }

    @Override
    public BlogEntity findByIdAndUserId(Long id, Long userId) {
        return blogRepository.findByIdAndUserId(id, userId)
                .orElseThrow(() -> new NotFoundException("must edit your blog"));
    }
}
