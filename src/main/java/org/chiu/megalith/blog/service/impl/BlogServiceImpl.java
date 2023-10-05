package org.chiu.megalith.blog.service.impl;

import com.aliyun.oss.OSS;
import com.aliyun.oss.OSSClientBuilder;
import jakarta.annotation.PostConstruct;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.chiu.megalith.blog.vo.*;
import org.chiu.megalith.infra.search.BlogIndexEnum;
import org.chiu.megalith.infra.search.BlogSearchIndexMessage;
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
import org.chiu.megalith.search.config.ElasticSearchRabbitConfig;
import org.springframework.amqp.rabbit.connection.CorrelationData;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.scheduling.annotation.Async;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.ByteArrayInputStream;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author mingchiuli
 * @create 2022-11-27 2:10 pm
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class BlogServiceImpl implements BlogService {

    private final BlogRepository blogRepository;

    private final StringRedisTemplate redisTemplate;

    private final JsonUtils jsonUtils;

    private final UserService userService;

    private final RabbitTemplate rabbitTemplate;

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

    @Value("${blog.highest-role}")
    private String highestRole;

    @Value("${blog.oss.access-key-id}")
    private String keyId;

    @Value("${blog.oss.access-key-secret}")
    private String keySecret;

    @Value("${blog.oss.bucket-name}")
    private String bucket;

    @Value("${blog.oss.endpoint}")
    private String ep;

    @Value("${blog.oss.host}")
    private String host;

    @Qualifier("imgUploadThreadPoolExecutor")
    private final ThreadPoolExecutor executor;

    private OSS ossClient;

    @PostConstruct
    private void init() {
        // Endpoint以华东1（杭州）为例，其它Region请按实际情况填写。
        String endpoint = ep;
        // 阿里云账号AccessKey拥有所有API的访问权限，风险很高。强烈建议您创建并使用RAM用户进行API访问或日常运维，请登录RAM控制台创建RAM用户。
        String accessKeyId = keyId;
        String accessKeySecret = keySecret;
        // 填写Object完整路径，例如exampledir/exampleobject.txt。Object完整路径中不能包含Bucket名称。
        // 创建OSSClient实例。
        ossClient = new OSSClientBuilder().build(endpoint, accessKeyId, accessKeySecret);
    }


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

    @SneakyThrows
    @Override
    public String uploadOss(MultipartFile image, Long userId) {
        Assert.notNull(image, "upload miss");
        String nickname = userService.findById(userId).getNickname();
        String uuid = UUID.randomUUID().toString();
        String originalFilename = image.getOriginalFilename();
        originalFilename = Optional.ofNullable(originalFilename)
                .orElseGet(() -> UUID.randomUUID().toString())
                .replace(" ", "");
        String objectName = nickname + "/" + uuid + "-" + originalFilename;
        byte[] imageBytes = image.getBytes();

        executor.execute(() -> ossClient.putObject(bucket, objectName, new ByteArrayInputStream(imageBytes)));
        //https://bloglmc.oss-cn-hangzhou.aliyuncs.com/admin/42166d224f4a20a45eca28b691529822730ed0ee.jpeg
        return host + "/" + objectName;
    }

    @Override
    public void deleteOss(String url) {
        String objectName = url.replace(host + "/", "");
        ossClient.deleteObject(bucket, objectName);
    }

    @Override
    public void setBlogStatus(Long id, Long userId, Integer status, String authority) {
        Long bdUserId = findUserIdById(id);

        if (Boolean.FALSE.equals(Objects.equals(highestRole, authority) || Objects.equals(userId, bdUserId))) {
            throw new BadCredentialsException("user unmatch");
        }

        Integer year = changeBlogStatus(id, status);

        var correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                BlogIndexEnum.UPDATE.name() + "_" + id,
                30,
                TimeUnit.MINUTES);

        rabbitTemplate.convertAndSend(ElasticSearchRabbitConfig.ES_EXCHANGE,
                ElasticSearchRabbitConfig.ES_BINDING_KEY,
                new BlogSearchIndexMessage(id, BlogIndexEnum.UPDATE, year),
                correlationData);
    }

    @Override
    public String setBlogToken(Long blogId) {
        Long dbUserId = findUserIdById(blogId);
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        long userId = Long.parseLong(authentication.getName());

        if (Objects.equals(userId, dbUserId)) {
            String token = UUID.randomUUID().toString();
            redisTemplate.opsForValue().set(Const.READ_TOKEN.getInfo() + blogId, token, 24, TimeUnit.HOURS);
            return token;
        }
        throw new BadCredentialsException("user mismatch");
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

        //通知消息给mq,更新并删除缓存
        //防止重复消费
        BlogIndexEnum type;
        if (Objects.nonNull(blog.getId())) {
            type = BlogIndexEnum.UPDATE;
        } else {
            type = BlogIndexEnum.CREATE;
        }

        var correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                type + "_" + blogEntity.getId(),
                30,
                TimeUnit.MINUTES);

        rabbitTemplate.convertAndSend(ElasticSearchRabbitConfig.ES_EXCHANGE,
                ElasticSearchRabbitConfig.ES_BINDING_KEY,
                new BlogSearchIndexMessage(blogEntity.getId(), type, blogEntity.getCreated().getYear()),
                correlationData);
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
    public void recoverDeletedBlog(Long id, Integer idx, Long userId) {

        String str = redisTemplate.opsForList().index(Const.QUERY_DELETED.getInfo() + userId, idx);

        BlogEntity tempBlog = jsonUtils.readValue(str, BlogEntity.class);
        BlogEntity blog = blogRepository.save(tempBlog);

        redisTemplate.opsForList().remove(Const.QUERY_DELETED.getInfo() + userId, 1, str);

        var correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                BlogIndexEnum.CREATE.name() + "_" + id,
                30,
                TimeUnit.MINUTES);

        rabbitTemplate.convertAndSend(ElasticSearchRabbitConfig.ES_EXCHANGE,
                ElasticSearchRabbitConfig.ES_BINDING_KEY,
                new BlogSearchIndexMessage(blog.getId(), BlogIndexEnum.CREATE, blog.getCreated().getYear()),
                correlationData);
    }

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
    @Transactional
    public void deleteBatch(List<Long> ids, Long userId, String authority) {
        ids.forEach(id -> {
            BlogEntity blogEntity = findById(id);

            if (Boolean.FALSE.equals(Objects.equals(blogEntity.getUserId(), userId)) && Boolean.FALSE.equals(Objects.equals(authority, highestRole))) {
                throw new BadCredentialsException("must delete own blog");
            }

            blogRepository.delete(blogEntity);

            blogEntity.setCreated(LocalDateTime.now());

            redisTemplate.execute(LuaScriptUtils.setBlogDeleteLua,
                    Collections.singletonList(Const.QUERY_DELETED.getInfo() + userId),
                    jsonUtils.writeValueAsString(blogEntity), "604800");

            //防止重复消费
            var correlationData = new CorrelationData();
            redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                    BlogIndexEnum.REMOVE.name() + "_" + id,
                    30,
                    TimeUnit.MINUTES);

            rabbitTemplate.convertAndSend(ElasticSearchRabbitConfig.ES_EXCHANGE,
                    ElasticSearchRabbitConfig.ES_BINDING_KEY,
                    new BlogSearchIndexMessage(id, BlogIndexEnum.REMOVE, blogEntity.getCreated().getYear()), correlationData);
        });
    }
}
