package org.chiu.megalith.blog.service.impl;

import lombok.SneakyThrows;
import org.chiu.megalith.blog.convertor.*;
import org.chiu.megalith.blog.dto.BlogExhibitDto;
import org.chiu.megalith.blog.event.BlogOperateEvent;
import org.chiu.megalith.blog.http.OssHttpService;
import org.chiu.megalith.blog.req.BlogEditPushAllReq;
import org.chiu.megalith.blog.vo.*;
import org.chiu.megalith.blog.wrapper.BlogWrapper;
import org.chiu.megalith.infra.key.KeyFactory;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.infra.search.BlogIndexEnum;
import org.chiu.megalith.infra.search.BlogSearchIndexMessage;
import org.chiu.megalith.infra.utils.*;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.repository.BlogRepository;
import org.chiu.megalith.blog.req.BlogEntityReq;
import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.repository.UserRepository;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.page.PageAdapter;
import lombok.RequiredArgsConstructor;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
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

import static org.chiu.megalith.infra.lang.Const.*;
import static org.chiu.megalith.infra.lang.ExceptionMessage.*;
import static org.chiu.megalith.blog.lang.MessageActionFieldEnum.*;


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

    private final OssHttpService ossHttpService;

    private final OssSignUtils ossSignUtils;

    private final BlogWrapper blogWrapper;

    private final ApplicationContext applicationContext;

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
        String password = redisTemplate.opsForValue().get(READ_TOKEN.getInfo() + blogId);
        if (StringUtils.hasLength(token) && StringUtils.hasLength(password)) {
            return password.equals(token);
        }
        return false;
    }

    @Override
    public BlogExhibitVo getBlogDetail(Authentication authentication, Long id) {

        BlogExhibitDto blogExhibitDto = blogWrapper.findById(id);
        Integer status = blogWrapper.findStatusById(id);

        if (StatusEnum.NORMAL.getCode().equals(status) ||
                (ROLE_PREFIX.getInfo() + highestRole).equals(SecurityUtils.getLoginRole())) {
            blogWrapper.setReadCount(id);
            return BlogExhibitVoConvertor.convert(blogExhibitDto);
        }

        if (authentication instanceof AnonymousAuthenticationToken) {
            throw new BadCredentialsException(AUTH_EXCEPTION.getMsg());
        }

        Long userId = Long.valueOf(authentication.getName());

        if (userId.equals(blogExhibitDto.getUserId())) {
            blogWrapper.setReadCount(id);
            return BlogExhibitVoConvertor.convert(blogExhibitDto);
        }

        throw new BadCredentialsException(AUTH_EXCEPTION.getMsg());
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

        if ((ROLE_PREFIX.getInfo() + highestRole).equals(SecurityUtils.getLoginRole())) {
            return StatusEnum.NORMAL.getCode();
        }

        String userId = authentication.getName();

        BlogEntity blog = blogRepository.findById(blogId)
                .orElseThrow(() -> new MissException(NO_FOUND));
        Long id = blog.getUserId();
        return Objects.equals(id, Long.valueOf(userId)) ?
                StatusEnum.NORMAL.getCode() :
                StatusEnum.HIDE.getCode();
    }

    @Override
    public void pushAll(BlogEditPushAllReq blog, Long userId) {
        Long id = blog.getId();
        String redisKey = Objects.isNull(id) ?
                TEMP_EDIT_BLOG.getInfo() + userId :
                TEMP_EDIT_BLOG.getInfo() + userId + ":" + id;

        String content = blog.getContent();

        List<String> paragraphList = List.of(content.split(PARAGRAPH_SPLITTER.getInfo()));
        String paragraphListString = jsonUtils.writeValueAsString(paragraphList);

        redisTemplate.execute(LuaScriptUtils.pushAllLua, Collections.singletonList(redisKey),
                paragraphListString, ID.getMsg(), USER_ID.getMsg(), TITLE.getMsg(), DESCRIPTION.getMsg(), STATUS.getMsg(), LINK.getMsg(), VERSION.getMsg(),
                Objects.isNull(blog.getId()) ? "" : blog.getId().toString(), userId.toString(), blog.getTitle(), blog.getDescription(), blog.getStatus().toString(), blog.getLink(), "-1",
                A_WEEK.getInfo());
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
        headers.put(HttpHeaders.AUTHORIZATION, ossSignUtils.getAuthorization(objectName, HttpMethod.PUT.name(), "image/jpg"));
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
        headers.put(HttpHeaders.AUTHORIZATION, ossSignUtils.getAuthorization(objectName, HttpMethod.DELETE.name(), ""));
        ossHttpService.deleteOssObject(objectName, headers);
    }

    @Override
    public String setBlogToken(Long blogId) {
        Long dbUserId = blogRepository.findUserIdById(blogId);
        Long userId = SecurityUtils.getLoginUserId();

        if (Objects.equals(userId, dbUserId)) {
            blogRepository.setStatus(blogId, StatusEnum.HIDE.getCode());
            String token = UUID.randomUUID().toString();
            redisTemplate.opsForValue().set(READ_TOKEN.getInfo() + blogId, token, 24, TimeUnit.HOURS);
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
            BlogExhibitDto blogExhibitDto = blogWrapper.findById(blogId);
            return BlogExhibitVoConvertor.convert(blogExhibitDto);
        }
        throw new BadCredentialsException(TOKEN_INVALID.getMsg());
    }

    @Override
    public List<Integer> searchYears() {
        Long count = Optional
                .ofNullable(
                        redisTemplate.execute(LuaScriptUtils.countYears, List.of(BLOOM_FILTER_YEARS.getInfo())))
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

        var blogSearchIndexMessage = new BlogSearchIndexMessage(blogId, type, blogEntity.getCreated().getYear());
        applicationContext.publishEvent(new BlogOperateEvent(this, blogSearchIndexMessage));
    }

    @Override
    @SuppressWarnings("all")
    public PageAdapter<BlogEntityVo> findAllABlogs(Integer currentPage, Integer size, Long userId, String role) {

        var pageRequest = PageRequest.of(currentPage - 1, size, Sort.by("created").descending());
        Page<BlogEntity> page = Objects.equals(role, ROLE_PREFIX.getInfo() + highestRole) ? blogRepository.findAll(pageRequest) :
                blogRepository.findAllByUserId(pageRequest, userId);

        List<BlogEntity> items = page.getContent();
        List<String> ids = items.stream()
                .map(item -> String.valueOf(item.getId()))
                .toList();

        List<String> res = redisTemplate.execute(LuaScriptUtils.getHotBlogsLua, 
                Collections.singletonList(HOT_READ.getInfo()), 
                jsonUtils.writeValueAsString(ids));

        Map<Long, Integer> readMap = new HashMap<>();
        for (int i = 0; i < res.size(); i += 2) {
            readMap.put(Long.valueOf(res.get(i)), Integer.valueOf(res.get(i + 1)));
        }

        return BlogEntityVoConvertor.convert(page, readMap, userId);
    }

    @Override
    @SuppressWarnings("unchecked")
    public PageAdapter<BlogDeleteVo> findDeletedBlogs(Integer currentPage, Integer size, Long userId) {

        List<BlogEntity> deletedBlogs = Optional
                .ofNullable(redisTemplate.opsForList().range(QUERY_DELETED.getInfo() + userId, 0, -1))
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

        List<String> resp = Optional.ofNullable(
                redisTemplate.execute(LuaScriptUtils.listDeletedRedisScript,
                        Collections.singletonList(QUERY_DELETED.getInfo() + userId),
                        String.valueOf(l), "-1", String.valueOf(size - 1), String.valueOf(start)))
                .orElseGet(ArrayList::new);

        List<String> respList = resp.subList(0, resp.size() - 1);
        Long total = Long.valueOf(resp.get(resp.size() - 1));

        List<BlogEntity> list = respList.stream()
                .map(str -> jsonUtils.readValue(str, BlogEntity.class))
                .toList();

        return BlogDeleteVoConvertor.convert(l, list, currentPage, size, total);
    }

    @Override
    public void recoverDeletedBlog(Integer idx, Long userId) {

        String str = Optional.ofNullable(
                redisTemplate.execute(LuaScriptUtils.recoverDeletedScript,
                        Collections.singletonList(QUERY_DELETED.getInfo() + userId),
                        String.valueOf(idx)))
                .orElse("");

        if (Boolean.FALSE.equals(StringUtils.hasLength(str))) {
            return;
        }

        BlogEntity tempBlog = jsonUtils.readValue(str, BlogEntity.class);
        BlogEntity blog = blogRepository.save(tempBlog);

        var blogSearchIndexMessage = new BlogSearchIndexMessage(blog.getId(), BlogIndexEnum.CREATE, blog.getCreated().getYear());
        applicationContext.publishEvent(new BlogOperateEvent(this, blogSearchIndexMessage));
    }

    @Override
    @SuppressWarnings("unchecked")
    public VisitStatisticsVo getVisitStatistics() {
        List<Long> list = Optional.ofNullable(redisTemplate.execute(LuaScriptUtils.getVisitLua,
                List.of(DAY_VISIT.getInfo(), WEEK_VISIT.getInfo(), MONTH_VISIT.getInfo(), YEAR_VISIT.getInfo())))
                .orElseGet(ArrayList::new);

        return VisitStatisticsVoConvertor.convert(list);
    }

    @Override
    public List<BlogHotReadVo> getScoreBlogs() {
        Set<ZSetOperations.TypedTuple<String>> set = redisTemplate.opsForZSet()
                .reverseRangeWithScores(HOT_READ.getInfo(), 0, 4);

        List<Long> ids = Optional.ofNullable(set).orElseGet(LinkedHashSet::new).stream()
                .map(item -> Long.valueOf(item.getValue()))
                .toList();

        List<BlogEntity> blogs = blogRepository.findAllById(ids);

        return BlogHotReadVoConvertor.convert(blogs, set);
    }

    @Override
    public BlogEditVo findEdit(Long id, Long userId) {

        String redisKey = KeyFactory.createBlogEditRedisKey(userId, id);
        Map<String, String> entries = redisTemplate.<String, String>opsForHash()
                .entries(redisKey);

        BlogEntity blog;
        int version;
        if (!entries.isEmpty()) {
            blog = BlogEntityConvertor.convert(entries);
            version = Integer.parseInt(entries.get(VERSION.getMsg()));

            entries.remove(ID.getMsg());
            entries.remove(USER_ID.getMsg());
            entries.remove(DESCRIPTION.getMsg());
            entries.remove(TITLE.getMsg());
            entries.remove(STATUS.getMsg());
            entries.remove(LINK.getMsg());
            entries.remove(VERSION.getMsg());

            StringBuilder content = new StringBuilder();

            for (int i = 1; i <= entries.size(); i++) {
                content.append(entries.get(PARAGRAPH_PREFIX.getInfo() + i));
                if (i != entries.size()) {
                    content.append(PARAGRAPH_SPLITTER.getInfo());
                }
            }

            blog.setContent(content.toString());
        } else if (Objects.isNull(id)) {
            // 新文章
            blog = BlogEntity.builder()
                    .status(StatusEnum.NORMAL.getCode())
                    .userId(userId)
                    .content("")
                    .description("")
                    .link("")
                    .title("")
                    .build();
            version = -1;

            redisTemplate.execute(LuaScriptUtils.pushAllLua, Collections.singletonList(redisKey),
                    "[]", ID.getMsg(), USER_ID.getMsg(), TITLE.getMsg(), DESCRIPTION.getMsg(), STATUS.getMsg(), LINK.getMsg(), VERSION.getMsg(),
                   "" , userId.toString(), "", "", StatusEnum.NORMAL.getCode().toString(), "", "-1",
                    A_WEEK.getInfo());
        } else {
            blog = blogRepository.findByIdAndUserId(id, userId)
                    .orElseThrow(() -> new MissException(EDIT_NO_AUTH));
            version = -1;
            List<String> paragraphList = List.of(blog.getContent().split(PARAGRAPH_SPLITTER.getInfo()));
            String paragraphListString = jsonUtils.writeValueAsString(paragraphList);

            redisTemplate.execute(LuaScriptUtils.pushAllLua, Collections.singletonList(redisKey),
                    paragraphListString, ID.getMsg(), USER_ID.getMsg(), TITLE.getMsg(), DESCRIPTION.getMsg(), STATUS.getMsg(), LINK.getMsg(), VERSION.getMsg(),
                    Objects.isNull(blog.getId()) ? "" : blog.getId().toString(), userId.toString(), blog.getTitle(), blog.getDescription(), blog.getStatus().toString(), blog.getLink(), "-1",
                    A_WEEK.getInfo());
        }

        return BlogEditVoConvertor.convert(blog, version);
    }

    @Override
    public void deleteBatch(List<Long> ids, Long userId, String role) {
        List<BlogEntity> blogList = new ArrayList<>();
        ids.forEach(id -> {
            BlogEntity blogEntity = blogRepository.findById(id)
                    .orElseThrow(() -> new MissException(NO_FOUND));
            if (Boolean.FALSE.equals(Objects.equals(blogEntity.getUserId(), userId))
                    && Boolean.FALSE.equals(Objects.equals(role, ROLE_PREFIX.getInfo() + highestRole))) {
                throw new BadCredentialsException(DELETE_NO_AUTH.getMsg());
            }
            blogList.add(blogEntity);
        });

        blogRepository.deleteAllById(ids);

        blogList.forEach(blogEntity -> {
            Long id = blogEntity.getId();
            redisTemplate.execute(LuaScriptUtils.setBlogDeleteLua,
                    Collections.singletonList(QUERY_DELETED.getInfo() + userId),
                    jsonUtils.writeValueAsString(blogEntity), A_WEEK.getInfo());

            var blogSearchIndexMessage = new BlogSearchIndexMessage(id, BlogIndexEnum.REMOVE, blogEntity.getCreated().getYear());
            applicationContext.publishEvent(new BlogOperateEvent(this, blogSearchIndexMessage));
        });

    }
}
