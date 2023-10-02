package org.chiu.megalith.blog.service.impl;

import org.chiu.megalith.blog.vo.*;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.chiu.megalith.infra.cache.Cache;
import org.chiu.megalith.blog.vo.BlogEntityVo;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.repository.BlogRepository;
import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.service.UserService;
import org.chiu.megalith.infra.exception.NotFoundException;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.infra.utils.JsonUtils;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.req.BlogEntityReq;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Stream;

/**
 * @author mingchiuli
 * @create 2022-11-27 2:10 pm
 */
@Service
@RequiredArgsConstructor
public class BlogServiceImpl implements BlogService {

    private final BlogRepository blogRepository;

    private final StringRedisTemplate redisTemplate;

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
        BlogEntity blogEntity = Boolean.TRUE.equals(visible) ?
                blogRepository.findByIdAndStatus(id, 0)
                        .orElseThrow(() -> new NotFoundException("blog not exist")) :
                blogRepository.findById(id)
                        .orElseThrow(() -> new NotFoundException("blog not exist"));

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
    public PageAdapter<BlogDescriptionVo> findPage(Integer currentPage, Integer year) {
        var pageRequest = PageRequest.of(currentPage - 1,
                blogPageSize,
                Sort.by("created").descending());

        Page<BlogEntity> page = Objects.equals(year, Integer.MIN_VALUE) ?
                blogRepository.findPage(pageRequest) :
                blogRepository.findPageByCreatedBetween(pageRequest, LocalDateTime.of(year, 1, 1 , 0, 0, 0), LocalDateTime.of(year, 12, 31 , 23, 59, 59));


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
        return blogRepository.countByCreatedBetween(LocalDateTime.of(year, 1, 1 , 0, 0, 0), LocalDateTime.of(year, 12, 31 , 23, 59, 59));
    }

    @Override
    public Boolean checkToken(Long blogId, String token) {
        token = token.trim();
        String password = redisTemplate.opsForValue().get(Const.READ_TOKEN.getInfo() + blogId);
        if (StringUtils.hasLength(token) && StringUtils.hasLength(password)) {
            return password.equals(token);
        }
        return false;
    }

    @Override
    @Cache(prefix = Const.HOT_BLOG)
    public Long findUserIdById(Long id) {
        return blogRepository.findUserIdById(id);
    }

    @Override
    public Integer checkStatusByIdAndUserId(Long blogId, Long userId) {
        BlogEntity blog = blogRepository.findById(blogId)
                .orElseThrow(() -> new NotFoundException("blog not exist"));
        Long id = blog.getUserId();   
        return Objects.equals(id, blogId) ? 0 : 1;  
    }

    @Override
    @Cache(prefix = Const.BLOG_STATUS)
    public Integer findStatusById(Long blogId) {
        return blogRepository.findStatusById(blogId);
    }

    @Override
    public List<Integer> searchYears() {
        Long count = Optional.ofNullable(redisTemplate.execute(LuaScriptUtils.countYears, List.of(Const.BLOOM_FILTER_YEARS.getInfo())))
                .orElse(0L);
        int start = 2021;
        int end = Math.max(start + count.intValue() - 1, start);
        var years = new ArrayList<Integer>(end - start + 1);
        for (int year = start; year <= end; year++) {
            years.add(year);
        }
        return years;
    }

    @Override
    public Long count() {
        return blogRepository.count();
    }

    @Override
    public BlogEntity saveOrUpdate(BlogEntityReq blog, Long userId) {
        Long blogId = blog.getId();

        BlogEntity blogEntity;

        if (Objects.nonNull(blogId)) {
            blogEntity = blogRepository.findById(blogId)
                    .orElseThrow(() -> new NotFoundException("blog not exist"));
            Assert.isTrue(Objects.equals(blogEntity.getUserId(), userId), "must edit your blog!");
        } else {
            blogEntity = BlogEntity.builder()
                    .created(LocalDateTime.now())
                    .userId(userId)
                    .readCount(0L)
                    .build();
        }

        BeanUtils.copyProperties(blog, blogEntity);
        blogRepository.save(blogEntity);
        return blogEntity;
    }

    @Override
    public PageAdapter<BlogEntityVo> findAllABlogs(Integer currentPage, Integer size, Long userId, String authority) {

        var pageRequest = PageRequest.of(currentPage - 1, size, Sort.by("created").descending());
        Page<BlogEntity> page = Objects.equals(authority, highestRole) ?
                blogRepository.findAll(pageRequest) :
                blogRepository.findAllByUserId(pageRequest, userId);

        List<BlogEntityVo> entities = page.getContent()
                .stream()
                .map(blogEntity ->
                        BlogEntityVo.builder()
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

        return PageAdapter.<BlogEntityVo>builder()
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
    public PageAdapter<BlogDeleteVo> findDeletedBlogs(Integer currentPage, Integer size, Long userId) {

        List<BlogEntity> deletedBlogs = Optional.ofNullable(redisTemplate.opsForList().range(Const.QUERY_DELETED.getInfo() + userId, 0, -1))
                .orElseGet(ArrayList::new).stream()
                        .map(blogStr -> jsonUtils.readValue(blogStr, BlogEntity.class))
                        .toList();

        int l = 0;
        for (BlogEntity blog : deletedBlogs) {
            if (LocalDateTime.now().minusDays(7).isAfter(blog.getCreated())) {
                l++;
            } else {
                break;
            }
        }

        int start = (currentPage - 1) * size;

        Long total = Optional.ofNullable(redisTemplate.execute(LuaScriptUtils.flushDelete,
                Collections.singletonList(Const.QUERY_DELETED.getInfo() + userId),
                String.valueOf(l), "-1"))
                        .orElse(0L);

        int totalPages = (int) (total % size == 0 ? total / size : total / size + 1);

        List<BlogEntity> rawList = Optional.ofNullable(redisTemplate.opsForList().range(Const.QUERY_DELETED.getInfo() + userId, start, start + size)).orElseGet(ArrayList::new).stream()
                .map(str -> jsonUtils.readValue(str, BlogEntity.class))
                .toList();

        List<BlogDeleteVo> content = new ArrayList<>();
        rawList.forEach(item -> content.add(BlogDeleteVo.builder()
                .link(item.getLink())
                .content(item.getContent())
                .readCount(item.getReadCount())
                .title(item.getTitle())
                .status(item.getStatus())
                .created(item.getCreated())
                .id(item.getId())
                .userId(item.getUserId())
                .description(item.getDescription())
                .build()));

        return PageAdapter.<BlogDeleteVo>builder()
                .content(content)
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
    public BlogEntity recoverDeletedBlog(Long id, Integer idx, Long userId) {

        String str = redisTemplate.opsForList().index(Const.QUERY_DELETED.getInfo() + userId, idx);

        BlogEntity tempBlog = jsonUtils.readValue(str, BlogEntity.class);
        BlogEntity blog = blogRepository.save(tempBlog);

        redisTemplate.opsForList().remove(Const.QUERY_DELETED.getInfo() + userId, 1, str);
        return blog;
    }

    @Override
    public Integer changeBlogStatus(Long id, Integer status) {
        int year = blogRepository.findById(id)
                .orElseThrow()
                .getCreated()
                .getYear();
        blogRepository.setStatus(id, status);
        return year;
    }

    @Override
    public Boolean exist(Long blogId) {
        return blogRepository.existsById(blogId);
    }

    @Override
    public VisitStatisticsVo getVisitStatistics() {
        List<Long> list = Optional.ofNullable(redisTemplate.execute(LuaScriptUtils.getVisitLua,
                List.of(Const.DAY_VISIT.getInfo(), Const.WEEK_VISIT.getInfo(), Const.MONTH_VISIT.getInfo(), 
                Const.YEAR_VISIT.getInfo())))
                        .orElseGet(ArrayList::new);

        return VisitStatisticsVo.builder()
                .dayVisit(list.get(0))
                .weekVisit(list.get(1))
                .monthVisit(list.get(2))
                .yearVisit(list.get(3))
                .build();
    }

    @Override
    public List<BlogHotReadVo> getScoreBlogs() {
        Set<ZSetOperations.TypedTuple<String>> set = redisTemplate.opsForZSet().reverseRangeWithScores(Const.HOT_READ.getInfo(), 0, 4);
        return Optional.ofNullable(set).orElseGet(HashSet::new).stream()
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

    @Override
    public void delete(BlogEntity blogEntity) {
        blogRepository.delete(blogEntity);
    }
}
