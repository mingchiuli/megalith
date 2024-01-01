package org.chiu.megalith.blog.service.impl;

import lombok.SneakyThrows;
import org.chiu.megalith.blog.http.OssHttpService;
import org.chiu.megalith.blog.vo.*;
import org.chiu.megalith.blog.wrapper.BlogWrapper;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.infra.search.BlogIndexEnum;
import org.chiu.megalith.infra.search.BlogSearchIndexMessage;
import org.chiu.megalith.infra.utils.*;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.repository.BlogRepository;
import org.chiu.megalith.blog.req.BlogEditPushAllReq;
import org.chiu.megalith.blog.req.BlogEntityReq;
import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.repository.UserRepository;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.page.PageAdapter;
import lombok.RequiredArgsConstructor;

import org.chiu.megalith.search.config.ElasticSearchRabbitConfig;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.http.HttpHeaders;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.server.header.CacheControlServerHttpHeadersWriter;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.TimeUnit;

import static org.chiu.megalith.infra.lang.ExceptionMessage.*;

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

    private final UserRepository userRepository;

    private final MessageUtils messageUtils;

    private final OssHttpService ossHttpService;

    private final OssSignUtils ossSignUtils;

    private final BlogWrapper blogWrapper;

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

    @Value("${blog.highest-role}")
    private String highestRole;

    @Value("${blog.oss.base-url}")
    private String baseUrl;


    public List<Long> findIds(Pageable pageRequest) {
        return blogRepository.findIds(pageRequest);
    }

    @Override
    public PageAdapter<BlogDescriptionVo> findPage(Integer currentPage, Integer year) {
        return blogWrapper.findPage(currentPage, year);
    }

    @Override
    public Integer getCountByYear(Integer year) {
        return blogWrapper.getCountByYear(year);
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
    public BlogExhibitVo getBlogDetail(Authentication authentication, Long id) {
        BlogExhibitVo blog;

        if (Boolean.FALSE.equals(authentication instanceof AnonymousAuthenticationToken)) {
            if ((Const.ROLE_PREFIX.getInfo() + highestRole).equals(SecurityUtils.getLoginAuthority())) {
                blog = blogWrapper.findById(id, true);
            } else {
                blog = blogWrapper.findById(id, false);
            }
        } else {
            blog = blogWrapper.findById(id, false);
        }
        blogWrapper.setReadCount(id);
        return blog;
    }

    @Override
    public Integer getBlogStatus(Authentication authentication, Long blogId) {
        Integer status = blogWrapper.findStatusById(blogId);

        if (StatusEnum.NORMAL.getCode().equals(status)) {
            return status;
        }

        if (Boolean.TRUE.equals(authentication instanceof AnonymousAuthenticationToken)) {
            return StatusEnum.HIDE.getCode();
        }

        if (("ROLE_" + highestRole).equals(SecurityUtils.getLoginAuthority())) {
            return StatusEnum.NORMAL.getCode();
        }

        String userId = authentication.getName();

        BlogEntity blog = blogRepository.findById(blogId)
                .orElseThrow(() -> new MissException(NO_FOUND));
        Long id = blog.getUserId();
        return Objects.equals(id, Long.valueOf(userId)) ? StatusEnum.NORMAL.getCode() : StatusEnum.HIDE.getCode();
    }

    @SneakyThrows
    @Override
    public String uploadOss(MultipartFile image, Long userId) {
        Assert.notNull(image, UPLOAD_MISS.getMsg());
        String uuid = UUID.randomUUID().toString();
        String originalFilename = image.getOriginalFilename();
        originalFilename = Optional.ofNullable(originalFilename)
                .orElseGet(() -> UUID.randomUUID().toString())
                .replace(" ", "");
        UserEntity user = userRepository.findById(userId)
                .orElseThrow(() -> new MissException(NO_FOUND));
        String objectName = user.getNickname() + "/" + uuid + "-" + originalFilename;
        byte[] imageBytes = image.getBytes();

        Map<String, String> headers = new HashMap<>();
        String gmtDate = ossSignUtils.getGMTDate();
        headers.put(HttpHeaders.DATE, gmtDate);
        headers.put(HttpHeaders.AUTHORIZATION, ossSignUtils.getAuthorization(objectName, "PUT", "image/jpg"));
        headers.put(HttpHeaders.CACHE_CONTROL, CacheControlServerHttpHeadersWriter.PRAGMA_VALUE);
        headers.put(HttpHeaders.CONTENT_TYPE, "image/jpg");
        ossHttpService.putOssObject(objectName, imageBytes, headers);
        // https://bloglmc.oss-cn-hangzhou.aliyuncs.com/admin/42166d224f4a20a45eca28b691529822730ed0ee.jpeg
        return baseUrl + "/" + objectName;
    }

    @Override
    public void deleteOss(String url) {
        String objectName = url.replace(baseUrl + "/", "");
        Map<String, String> headers = new HashMap<>();
        String gmtDate = ossSignUtils.getGMTDate();
        headers.put(HttpHeaders.DATE, gmtDate);
        headers.put(HttpHeaders.AUTHORIZATION, ossSignUtils.getAuthorization(objectName, "DELETE", ""));
        ossHttpService.deleteOssObject(objectName, headers);
    }

    @Override
    public String setBlogToken(Long blogId) {
        Long dbUserId = blogRepository.findUserIdById(blogId);
        Long userId = SecurityUtils.getLoginUserId();

        if (Objects.equals(userId, dbUserId)) {
            blogRepository.setStatus(blogId, StatusEnum.HIDE.getCode());
            String token = UUID.randomUUID().toString();
            redisTemplate.opsForValue().set(Const.READ_TOKEN.getInfo() + blogId, token, 24, TimeUnit.HOURS);
            return token;
        }
        throw new BadCredentialsException(USER_MISS.getMsg());
    }

    @Override
    public List<Integer> getYears() {
        return blogRepository.getYears();
    }

    @Override
    public BlogExhibitVo getLockedBlog(Long blogId, String token) {
        boolean valid = checkToken(blogId, token);
        if (valid) {
            blogWrapper.setReadCount(blogId);
            return blogWrapper.findById(blogId, true);
        }
        throw new BadCredentialsException(TOKEN_INVALID.getMsg());
    }

    @Override
    public List<Integer> searchYears() {
        Long count = Optional
                .ofNullable(
                        redisTemplate.execute(LuaScriptUtils.countYears, List.of(Const.BLOOM_FILTER_YEARS.getInfo())))
                .orElse(0L);
        int start = 2021;
        int end = Math.max(start + count.intValue() - 1, start);
        var years = new ArrayList<Integer>(end - start + 1);
        for (int year = start; year <= end; year++) {
            years.add(year);
        }
        if (years.size() == 1) {
            years.add(start);
        }
        return years;
    }

    @Override
    public Long count() {
        return blogRepository.count();
    }

    @Override
    public void saveOrUpdate(BlogEntityReq blog, Long userId) {
        Long blogId = blog.getId();
        BlogEntity blogEntity;

        LocalDateTime now = LocalDateTime.now();
        if (Objects.nonNull(blogId)) {
            blogEntity = blogRepository.findById(blogId)
                    .orElseThrow(() -> new MissException(NO_FOUND));
            Assert.isTrue(Objects.equals(blogEntity.getUserId(), userId), EDIT_NO_AUTH.getMsg());
        } else {
            blogEntity = BlogEntity.builder()
                    .created(now)
                    .userId(userId)
                    .readCount(0L)
                    .build();
        }

        blogEntity.setUpdated(now);
        BeanUtils.copyProperties(blog, blogEntity);
        BlogEntity saved = blogRepository.save(blogEntity);

        // 通知消息给mq,更新并删除缓存
        // 防止重复消费
        BlogIndexEnum type;
        if (Objects.nonNull(blogId)) {
            type = BlogIndexEnum.UPDATE;
        } else {
            type = BlogIndexEnum.CREATE;
            blogId = saved.getId();
        }

        messageUtils.sendMessageOnce(ElasticSearchRabbitConfig.ES_EXCHANGE,
                ElasticSearchRabbitConfig.ES_BINDING_KEY,
                new BlogSearchIndexMessage(blogId, type, blogEntity.getCreated().getYear()),
                type.name(), blogId);
    }

    @Override
    public PageAdapter<BlogEntityVo> findAllABlogs(Integer currentPage, Integer size, Long userId, String authority) {

        var pageRequest = PageRequest.of(currentPage - 1, size, Sort.by("created").descending());
        Page<BlogEntity> page = Objects.equals(authority, highestRole) ? blogRepository.findAll(pageRequest)
                : blogRepository.findAllByUserId(pageRequest, userId);

        List<BlogEntityVo> entities = page.getContent()
                .stream()
                .map(blogEntity -> BlogEntityVo.builder()
                        .id(blogEntity.getId())
                        .title(blogEntity.getTitle())
                        .description(blogEntity.getDescription())
                        .readCount(blogEntity.getReadCount())
                        .recentReadCount(
                                Optional.ofNullable(redisTemplate.opsForZSet().score(Const.HOT_READ.getInfo(),
                                        blogEntity.getId().toString()))
                                        .orElse(0.0))
                        .status(blogEntity.getStatus())
                        .link(blogEntity.getLink())
                        .created(blogEntity.getCreated())
                        .updated(blogEntity.getUpdated())
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
    @SuppressWarnings("unchecked")
    public PageAdapter<BlogDeleteVo> findDeletedBlogs(Integer currentPage, Integer size, Long userId) {

        List<BlogEntity> deletedBlogs = Optional
                .ofNullable(redisTemplate.opsForList().range(Const.QUERY_DELETED.getInfo() + userId, 0, -1))
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

        List resp = Optional.ofNullable(
                redisTemplate.execute(LuaScriptUtils.listDeletedRedisScript,
                        Collections.singletonList(Const.QUERY_DELETED.getInfo() + userId),
                        String.valueOf(l), "-1", String.valueOf(size - 1), String.valueOf(start)))
                .orElseGet(ArrayList::new);

        List<String> respList = resp.subList(0, resp.size() - 1);
        Long total = (long) resp.get(resp.size() - 1);
        int totalPages = (int) (total % size == 0 ? total / size : total / size + 1);

        List<BlogEntity> list = respList.stream()
                .map(str -> jsonUtils.readValue(str, BlogEntity.class))
                .toList();

        List<BlogDeleteVo> content = new ArrayList<>();
        for (BlogEntity item : list) {
            content.add(BlogDeleteVo.builder()
                    .idx(l++)
                    .link(item.getLink())
                    .content(item.getContent())
                    .readCount(item.getReadCount())
                    .title(item.getTitle())
                    .status(item.getStatus())
                    .created(item.getCreated())
                    .updated(item.getUpdated())
                    .id(item.getId())
                    .userId(item.getUserId())
                    .description(item.getDescription())
                    .build());
        }

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
    public void recoverDeletedBlog(Long id, Integer idx, Long userId) {

        String str = Optional.ofNullable(
                redisTemplate.execute(LuaScriptUtils.recoverDeletedScript,
                        Collections.singletonList(Const.QUERY_DELETED.getInfo() + userId),
                        String.valueOf(idx)))
                .orElse("");

        if (Boolean.FALSE.equals(StringUtils.hasLength(str))) {
            return;
        }

        BlogEntity tempBlog = jsonUtils.readValue(str, BlogEntity.class);
        BlogEntity blog = blogRepository.save(tempBlog);

        messageUtils.sendMessageOnce(ElasticSearchRabbitConfig.ES_EXCHANGE,
                ElasticSearchRabbitConfig.ES_BINDING_KEY,
                new BlogSearchIndexMessage(blog.getId(), BlogIndexEnum.CREATE, blog.getCreated().getYear()),
                BlogIndexEnum.CREATE.name(), id);
    }

    @Override
    @SuppressWarnings("unchecked")
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
        Set<ZSetOperations.TypedTuple<String>> set = redisTemplate.opsForZSet()
                .reverseRangeWithScores(Const.HOT_READ.getInfo(), 0, 4);
        List<BlogHotReadVo> items = Optional.ofNullable(set).orElseGet(LinkedHashSet::new).stream()
                .map(item -> BlogHotReadVo.builder()
                        .id(Long.valueOf(item.getValue()))
                        .readCount(item.getScore().longValue())
                        .build())
                .toList();

        List<Long> ids = items.stream()
                .map(BlogHotReadVo::getId)
                .toList();
        List<BlogEntity> blogs = blogRepository.findAllById(ids);
        items.forEach(item -> {
            String title = blogs.stream()
                    .filter(blog -> blog.getId().equals(item.getId()))
                    .findAny()
                    .orElseThrow(() -> new MissException(NO_FOUND))
                    .getTitle();
            item.setTitle(title);
        });

        return items;
    }

    @Override
    public BlogEditVo findEdit(Long id, Long userId) {
        String str = redisTemplate.<String, String>opsForHash()
                .get(Objects.isNull(id) ? Const.TEMP_EDIT_BLOG.getInfo() + userId
                        : Const.TEMP_EDIT_BLOG.getInfo() + userId + ":" + id, "blog");
        // 暂存区
        BlogEntity blogEntity;
        BlogEditPushAllReq req;
        if (StringUtils.hasLength(str)) {
            req = jsonUtils.readValue(str, BlogEditPushAllReq.class);
            blogEntity = BlogEntity.builder()
                    .id(req.getId())
                    .content(req.getContent())
                    .description(req.getDescription())
                    .title(req.getTitle())
                    .status(req.getStatus())
                    .link(req.getLink())
                    .build();
        } else if (Objects.isNull(id)) {
            // 新文章
            blogEntity = BlogEntity.builder()
                    .status(0)
                    .content("")
                    .description("")
                    .link("")
                    .title("")
                    .build();
        } else {
            blogEntity = blogRepository.findByIdAndUserId(id, userId)
                    .orElseThrow(() -> new MissException(EDIT_NO_AUTH));
        }

        // 初始化暂存区
        String redisKey = Objects.isNull(id) ? Const.TEMP_EDIT_BLOG.getInfo() + userId
                : Const.TEMP_EDIT_BLOG.getInfo() + userId + ":" + id;
        BlogEditPushAllReq blog = BlogEditPushAllReq.builder()
                .id(blogEntity.getId())
                .title(blogEntity.getTitle())
                .description(blogEntity.getDescription())
                .content(blogEntity.getContent())
                .link(blogEntity.getLink())
                .status(blogEntity.getStatus())
                .build();
        redisTemplate.execute(LuaScriptUtils.sendBlogToTempLua, Collections.singletonList(redisKey),
                "blog", "version", jsonUtils.writeValueAsString(blog), "-1", "604800");

        return BlogEditVo.builder()
                .id(blogEntity.getId())
                .title(blogEntity.getTitle())
                .description(blogEntity.getDescription())
                .content(blogEntity.getContent())
                .link(blogEntity.getLink())
                .status(blogEntity.getStatus())
                .build();
    }

    @Override
    public void deleteBatch(List<Long> ids, Long userId, String authority) {
        List<BlogEntity> blogList = new ArrayList<>();
        ids.forEach(id -> {
            BlogEntity blogEntity = blogRepository.findById(id)
                    .orElseThrow(() -> new MissException(NO_FOUND));
            if (Boolean.FALSE.equals(Objects.equals(blogEntity.getUserId(), userId))
                    && Boolean.FALSE.equals(Objects.equals(authority, highestRole))) {
                throw new BadCredentialsException(DELETE_NO_AUTH.getMsg());
            }
            blogList.add(blogEntity);
        });

        blogRepository.deleteAllById(ids);

        blogList.forEach(blogEntity -> {
            Long id = blogEntity.getId();
            redisTemplate.execute(LuaScriptUtils.setBlogDeleteLua,
                    Collections.singletonList(Const.QUERY_DELETED.getInfo() + userId),
                    jsonUtils.writeValueAsString(blogEntity), "604800");

            messageUtils.sendMessageOnce(ElasticSearchRabbitConfig.ES_EXCHANGE,
                    ElasticSearchRabbitConfig.ES_BINDING_KEY,
                    new BlogSearchIndexMessage(id, BlogIndexEnum.REMOVE, blogEntity.getCreated().getYear()),
                    BlogIndexEnum.REMOVE.name(), id);
        });

    }
}
