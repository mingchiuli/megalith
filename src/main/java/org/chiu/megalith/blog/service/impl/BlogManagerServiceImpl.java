package org.chiu.megalith.blog.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.blog.http.OssHttpService;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.infra.constant.BlogOperateEnum;
import org.chiu.megalith.infra.constant.BlogOperateMessage;
import org.chiu.megalith.infra.utils.JsonUtils;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.chiu.megalith.infra.utils.OssSignUtils;
import org.chiu.megalith.infra.utils.SecurityUtils;
import org.chiu.megalith.blog.convertor.BlogDeleteVoConvertor;
import org.chiu.megalith.blog.convertor.BlogEntityVoConvertor;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.user.entity.UserEntity;
import org.chiu.megalith.blog.event.BlogOperateEvent;
import org.chiu.megalith.blog.repository.BlogRepository;
import org.chiu.megalith.user.repository.UserRepository;
import org.chiu.megalith.blog.req.BlogEntityReq;
import org.chiu.megalith.blog.service.BlogManagerService;
import org.chiu.megalith.blog.vo.BlogDeleteVo;
import org.chiu.megalith.blog.vo.BlogEntityVo;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.web.server.header.CacheControlServerHttpHeadersWriter;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

import static org.chiu.megalith.infra.lang.Const.*;
import static org.chiu.megalith.infra.lang.ExceptionMessage.*;

@Service
@RequiredArgsConstructor
public class BlogManagerServiceImpl implements BlogManagerService {


    private final JsonUtils jsonUtils;

    private final UserRepository userRepository;

    private final OssHttpService ossHttpService;

    private final OssSignUtils ossSignUtils;

    private final ApplicationContext applicationContext;

    private final BlogRepository blogRepository;

    private final StringRedisTemplate redisTemplate;

    private final ObjectMapper objectMapper;

    private final SecurityUtils securityUtils;

    @Qualifier("commonExecutor")
    private final ExecutorService taskExecutor;

    @Value("${blog.oss.base-url}")
    private String baseUrl;

    @SneakyThrows
    @Override
    public void download(HttpServletResponse response) {
        ServletOutputStream outputStream = response.getOutputStream();
        response.setCharacterEncoding(StandardCharsets.UTF_8.name());

        Set<BlogEntity> items = Collections.newSetFromMap(new ConcurrentHashMap<>());
        List<CompletableFuture<Void>> completableFutures = new ArrayList<>();
        long total = blogRepository.count();
        int pageSize = 20;
        int totalPage = (int) (total % pageSize == 0 ? total / pageSize : total / pageSize + 1);

        for (int i = 1; i <= totalPage; i++) {
            PageRequest pageRequest = PageRequest.of(i, pageSize);
            CompletableFuture<Void> completableFuture = CompletableFuture.runAsync(() -> {
                Page<BlogEntity> page = blogRepository.findAll(pageRequest);
                items.addAll(page.getContent());
            }, taskExecutor);
            completableFutures.add(completableFuture);
        }

        CompletableFuture.allOf(completableFutures.toArray(new CompletableFuture[0])).get(1000, TimeUnit.MILLISECONDS);
        BlogEntity[] blogs = items.toArray(new BlogEntity[0]);
        int len = blogs.length;

        for (int i = 0; i < len; i++) {
            if (i == 0) {
                //[
                outputStream.write(new byte[]{91});
            }

            byte[] bytes = objectMapper.writeValueAsBytes(blogs[i]);
            outputStream.write(bytes);
            if (i != len - 1) {
                //,
                outputStream.write(new byte[]{44});
            }

            if (i == len - 1) {
                //]
                outputStream.write(new byte[]{93});
            }
        }
        outputStream.flush();
        outputStream.close();
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
            String token = UUID.randomUUID().toString();
            redisTemplate.opsForValue().set(READ_TOKEN.getInfo() + blogId, token, 24, TimeUnit.HOURS);
            return token;
        }
        throw new BadCredentialsException(USER_MISS.getMsg());
    }

    @Override
    public void saveOrUpdate(BlogEntityReq blog, Long userId) {
        Long blogId = blog.getId();
        BlogEntity blogEntity;

        if (Objects.nonNull(blogId)) {
            blogEntity = blogRepository.findById(blogId)
                    .orElseThrow(() -> new MissException(NO_FOUND));
            Assert.isTrue(Objects.equals(blogEntity.getUserId(), userId), EDIT_NO_AUTH.getMsg());
        } else {
            blogEntity = BlogEntity.builder()
                    .userId(userId)
                    .readCount(0L)
                    .build();
        }

        BeanUtils.copyProperties(blog, blogEntity);
        BlogEntity saved = blogRepository.save(blogEntity);

        // 通知消息给mq,更新并删除缓存
        // 防止重复消费
        BlogOperateEnum type;
        if (Objects.nonNull(blogId)) {
            type = BlogOperateEnum.UPDATE;
        } else {
            type = BlogOperateEnum.CREATE;
            blogId = saved.getId();
        }

        var blogSearchIndexMessage = new BlogOperateMessage(blogId, type, blogEntity.getCreated().getYear());
        applicationContext.publishEvent(new BlogOperateEvent(this, blogSearchIndexMessage));
    }

    @Override
    @SuppressWarnings("all")
    public PageAdapter<BlogEntityVo> findAllABlogs(Integer currentPage, Integer size, Long userId, List<String> roles) {

        var pageRequest = PageRequest.of(currentPage - 1, size, Sort.by("created").descending());
        Page<BlogEntity> page = securityUtils.isAdmin(roles) ? blogRepository.findAll(pageRequest) :
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
        Long total = Long.valueOf(resp.getLast());

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

        var blogSearchIndexMessage = new BlogOperateMessage(blog.getId(), BlogOperateEnum.CREATE, blog.getCreated().getYear());
        applicationContext.publishEvent(new BlogOperateEvent(this, blogSearchIndexMessage));
    }

    @Override
    public void deleteBatch(List<Long> ids, Long userId, List<String> roles) {
        List<BlogEntity> blogList = new ArrayList<>();
        ids.forEach(id -> {
            BlogEntity blogEntity = blogRepository.findById(id)
                    .orElseThrow(() -> new MissException(NO_FOUND));
            if (Boolean.FALSE.equals(Objects.equals(blogEntity.getUserId(), userId)) && Boolean.FALSE.equals(securityUtils.isAdmin(roles))) {
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

            var blogSearchIndexMessage = new BlogOperateMessage(id, BlogOperateEnum.REMOVE, blogEntity.getCreated().getYear());
            applicationContext.publishEvent(new BlogOperateEvent(this, blogSearchIndexMessage));
        });

    }
}
