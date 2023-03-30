package com.chiu.megalith.exhibit.service.impl;

import com.chiu.megalith.exhibit.cache.Cache;
import com.chiu.megalith.exhibit.dto.BlogEntityDto;
import com.chiu.megalith.exhibit.entity.BlogEntity;
import com.chiu.megalith.exhibit.repository.BlogRepository;
import com.chiu.megalith.exhibit.service.BlogService;
import com.chiu.megalith.exhibit.vo.BlogExhibitVo;
import com.chiu.megalith.exhibit.vo.BlogHotReadVo;
import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.service.UserService;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.data.redis.core.script.RedisScript;
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

    public List<Long> findIdsByStatus(Integer status, Pageable pageRequest) {
        return blogRepository.findIdsByStatus(status, pageRequest);
    }

    @Cache(prefix = Const.HOT_BLOG)
    public BlogExhibitVo findByIdAndVisible(Long id) {
        BlogEntity blogEntity = blogRepository.findByIdAndStatus(id, 0)
                .orElseThrow(() -> new NotFoundException("blog not found"));
        UserEntity user = userService.findById(blogEntity.getUserId());

        return BlogExhibitVo.builder()
                .title(blogEntity.getTitle())
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
    public PageAdapter<BlogEntity> findPage(Integer currentPage) {
        Pageable pageRequest = PageRequest.of(currentPage - 1,
                blogPageSize,
                Sort.by("created").descending());
        Page<BlogEntity> page = blogRepository.findPage(pageRequest);
        return new PageAdapter<>(page);
    }

    @Override
    public PageAdapter<BlogEntity> findPageByYear(Integer currentPage,
                                                  Integer year) {
        LocalDateTime start = LocalDateTime.of(year, 1, 1 , 0, 0, 0);
        LocalDateTime end = LocalDateTime.of(year, 12, 31 , 23, 59, 59);
        Pageable pageRequest = PageRequest.of(currentPage - 1,
                blogPageSize,
                Sort.by("created").descending());
        Page<BlogEntity> page = blogRepository.findPageByCreatedBetween(pageRequest, start, end);
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
                return blogRepository.findById(blogId)
                        .orElseThrow(() -> new NotFoundException("status error"));
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
            ref.blogEntity = blogRepository.findById(blog.getId())
                    .orElseThrow(() -> new NotFoundException("blog not exist"));
            Assert.isTrue(ref.blogEntity.getUserId().equals(userId), "must edit your blog!");
            ref.type = BlogIndexEnum.UPDATE;
        }, () -> {
            ref.blogEntity = BlogEntity.builder()
                    .created(LocalDateTime.now())
                    .userId(userId)
                    .readCount(0L)
                    .build();
            ref.type = BlogIndexEnum.CREATE;
        });

        BeanUtils.copyProperties(blog, ref.blogEntity);
        blogRepository.save(ref.blogEntity);

        //通知消息给mq,更新并删除缓存
        //防止重复消费
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
        String authority = authentication.getAuthorities().stream()
                .findFirst()
                .map(GrantedAuthority::getAuthority)
                .orElseThrow();
        long userId = Long.parseLong(authentication.getName());

        ids.forEach(id -> {
            BlogEntity blogEntity = blogRepository.findById(id)
                    .orElseThrow(() -> new NotFoundException("blog not exist"));

            if (!blogEntity.getUserId().equals(userId) && !authority.equals(highestRole)) {
                throw new AuthenticationExceptionImpl("must delete own blog");
            }

            blogRepository.delete(blogEntity);

            redisTemplate.opsForValue().set(userId + Const.QUERY_DELETED.getInfo() + id,
                    jsonUtils.writeValueAsString(blogEntity),
                    7,
                    TimeUnit.DAYS);

            //防止重复消费
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
                )
                .orElse("read token is not exist");
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

        Set<String> keys = redisTemplate.keys(userId + Const.QUERY_DELETED.getInfo() + "*");

        int total = keys.size();
        int totalPages = total % size == 0 ? total / size : total / size + 1;

        List<String> list = redisTemplate.opsForValue().multiGet(keys);

        return PageAdapter.<BlogEntity>builder()
                .content(list.stream()
                        .filter(Objects::nonNull)
                        .map(str -> jsonUtils.readValue(str, BlogEntity.class))
                        .sorted((o1, o2) -> o2.getCreated().compareTo(o1.getCreated()))
                        .limit((long) currentPage * size)
                        .skip((long) (currentPage - 1) * size)
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
    public void recoverDeletedBlog(Long id) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        String blogStr = Optional.ofNullable(
                redisTemplate.opsForValue().get(userId + Const.QUERY_DELETED.getInfo() + id)
                )
                .orElseThrow(() -> new NotFoundException("blog is expired"));

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

    private final RedisScript<List> getVisitLua = RedisScript.of(
            "local daySize = redis.call('pfcount', KEYS[1]);" +
            "local weekSize = redis.call('pfcount', KEYS[2]);" +
            "local monthSize = redis.call('pfcount', KEYS[3]);" +
            "local yearSize = redis.call('pfcount', KEYS[4]);" +
            "local resp = {};" +
            "table.insert(resp, daySize);" +
            "table.insert(resp, weekSize);" +
            "table.insert(resp, monthSize);" +
            "table.insert(resp, yearSize);" +
            "return resp;",
            List.class);

    @Override
    public Map<String, Long> getVisitStatistics() {
        List<Long> list = redisTemplate.execute(getVisitLua,
                Arrays.asList(Const.DAY_VISIT.getInfo(), Const.WEEK_VISIT.getInfo(), Const.MONTH_VISIT.getInfo(), Const.YEAR_VISIT.getInfo()));

        Map<String, Long> map = new HashMap<>(7);
        map.put("daySize", list.get(0));
        map.put("weekSize", list.get(1));
        map.put("monthSize", list.get(2));
        map.put("yearSize", list.get(3));

        return map;
    }

    @Override
    public List<BlogHotReadVo> getScoreBlogs() {
        Set<ZSetOperations.TypedTuple<String>> set = redisTemplate.opsForZSet().reverseRangeWithScores(Const.HOT_READ.getInfo(), 0, 4);
        return set.stream()
                .map(item -> {
                    Long id = Long.valueOf(item.getValue());
                    return BlogHotReadVo.builder()
                            .id(id)
                            .readCount(item.getScore().longValue())
                            .build();
                })
                .toList();
    }

    @Override
    @Cache(prefix = Const.HOT_BLOG)
    public String findTitleById(Long id) {
        return blogRepository.findTitleById(id)
                .orElseThrow();
    }
}
