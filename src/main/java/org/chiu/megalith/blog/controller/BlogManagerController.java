package org.chiu.megalith.blog.controller;

import org.chiu.megalith.blog.vo.BlogDeleteVo;
import org.chiu.megalith.blog.vo.BlogEntityVo;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.manage.req.BlogEntityReq;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.infra.search.BlogIndexEnum;
import org.chiu.megalith.infra.search.BlogSearchIndexMessage;
import org.chiu.megalith.infra.utils.JsonUtils;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.chiu.megalith.search.config.ElasticSearchRabbitConfig;

import lombok.RequiredArgsConstructor;

import org.springframework.amqp.rabbit.connection.CorrelationData;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

/**
 * @author mingchiuli
 * @create 2022-12-01 9:28 pm
 */
@RestController
@RequiredArgsConstructor
@RequestMapping(value = "/sys/blog")
public class BlogManagerController {

    private final BlogService blogService;

    private final StringRedisTemplate redisTemplate;

    private final RabbitTemplate rabbitTemplate;

    private final JsonUtils jsonUtils;

    @Value("${blog.highest-role}")
    private String highestRole;

    @GetMapping("/echo/{id}")
    public Result<BlogEntity> getEchoDetail(@PathVariable(name = "id") Long id) {
        Long userId = Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());
        return Result.success(() -> blogService.findByIdAndUserId(id, userId));
    }

    @PostMapping("/save")
    public Result<Void> saveOrUpdate(@RequestBody @Validated BlogEntityReq blog) {
        Long userId = Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());
        BlogEntity blogEntity = blogService.saveOrUpdate(blog, userId);
        //通知消息给mq,更新并删除缓存
        //防止重复消费
        BlogIndexEnum type;
        Long blogId = blog.getId();

        if (Objects.nonNull(blogId)) {
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
        return Result.success();
    }

    @PostMapping("/delete")
    @Transactional
    public Result<Void> deleteBlogs(@RequestBody List<Long> ids) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String authority = authentication.getAuthorities().stream()
                .findFirst()
                .map(GrantedAuthority::getAuthority)
                .orElseThrow();
        long userId = Long.parseLong(authentication.getName());

        ids.forEach(id -> {
            BlogEntity blogEntity = blogService.findById(id);

            if (Boolean.FALSE.equals(Objects.equals(blogEntity.getUserId(), userId)) && Boolean.FALSE.equals(Objects.equals(authority, highestRole))) {
                throw new BadCredentialsException("must delete own blog");
            }

            blogService.delete(blogEntity);

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

        return Result.success();
    }

    @GetMapping("/lock/{blogId}")
    public Result<String> setBlogToken(@PathVariable(value = "blogId") Long blogId) {
        Long dbUserId = blogService.findUserIdById(blogId);
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        long userId = Long.parseLong(authentication.getName());

        if (Objects.equals(userId, dbUserId)) {
            String token = UUID.randomUUID().toString();
            redisTemplate.opsForValue().set(Const.READ_TOKEN.getInfo() + blogId, token, 24, TimeUnit.HOURS);
            return Result.success(token);
        }
        throw new BadCredentialsException("user mismatch");
    }

    @GetMapping("/blogs")
    public Result<PageAdapter<BlogEntityVo>> getAllBlogs(@RequestParam(defaultValue = "1") Integer currentPage,
                                                         @RequestParam(defaultValue = "5") Integer size) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        Long userId = Long.valueOf(authentication.getName());
        String authority = authentication.getAuthorities().stream()
                .findFirst()
                .map(GrantedAuthority::getAuthority)
                .orElseThrow();

        return Result.success(() -> blogService.findAllABlogs(currentPage, size, userId, authority));
    }

    @GetMapping("/deleted")
    public Result<PageAdapter<BlogDeleteVo>> getDeletedBlogs(@RequestParam Integer currentPage,
                                                             @RequestParam Integer size) {
        Long userId = Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());
        return Result.success(() -> blogService.findDeletedBlogs(currentPage, size, userId));
    }

    @GetMapping("/recover/{id}/{idx}")
    public Result<Void> recoverDeletedBlog(@PathVariable(value = "id") Long id,
                                           @PathVariable(value = "idx") Integer idx) {
        Long userId = Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());

        BlogEntity blog = blogService.recoverDeletedBlog(id, idx, userId);

        var correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                BlogIndexEnum.CREATE.name() + "_" + id,
                30,
                TimeUnit.MINUTES);

        rabbitTemplate.convertAndSend(ElasticSearchRabbitConfig.ES_EXCHANGE,
                ElasticSearchRabbitConfig.ES_BINDING_KEY,
                new BlogSearchIndexMessage(blog.getId(), BlogIndexEnum.CREATE, blog.getCreated().getYear()),
                correlationData);
        return Result.success();
    }

    @GetMapping("/status/{id}/{status}")
    public Result<Void> setBlogStatus(@PathVariable(value = "id") Long id,
                                      @PathVariable(value = "status") Integer status) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String authority = authentication.getAuthorities().stream()
                .findFirst()
                .map(GrantedAuthority::getAuthority)
                .orElseThrow();

        Long userId = Long.valueOf(authentication.getName());
        Long bdUserId = blogService.findUserIdById(id);

        if (Boolean.FALSE.equals(Objects.equals(highestRole, authority) || Objects.equals(userId, bdUserId))) {
            throw new BadCredentialsException("user unmatch");
        }

        Integer year = blogService.changeBlogStatus(id, status);
        
        var correlationData = new CorrelationData();
        redisTemplate.opsForValue().set(Const.CONSUME_MONITOR.getInfo() + correlationData.getId(),
                BlogIndexEnum.UPDATE.name() + "_" + id,
                30,
                TimeUnit.MINUTES);

        rabbitTemplate.convertAndSend(ElasticSearchRabbitConfig.ES_EXCHANGE,
                ElasticSearchRabbitConfig.ES_BINDING_KEY,
                new BlogSearchIndexMessage(id, BlogIndexEnum.UPDATE, year),
                correlationData);

        return Result.success();
    }

    @PostMapping("/oss/upload")
    public Result<String> uploadOss(@RequestParam MultipartFile image) {
        return Result.success(() -> blogService.uploadOss(image));
    }

    @PostMapping("/oss/delete")
    public Result<Void> deleteOss(@RequestParam String url) {
        return Result.success(() -> blogService.deleteOss(url));
    }
}
