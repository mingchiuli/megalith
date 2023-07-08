package com.chiu.megalith.blog.controller;

import com.chiu.megalith.blog.dto.BlogEntityDto;
import com.chiu.megalith.blog.entity.BlogEntity;
import com.chiu.megalith.blog.service.BlogService;
import com.chiu.megalith.blog.vo.BlogEntityVo;
import com.chiu.megalith.infra.exception.AuthenticationExceptionImpl;
import com.chiu.megalith.infra.lang.Const;
import com.chiu.megalith.infra.lang.Result;
import com.chiu.megalith.infra.page.PageAdapter;
import com.chiu.megalith.infra.search.BlogIndexEnum;
import com.chiu.megalith.infra.search.BlogSearchIndexMessage;
import com.chiu.megalith.infra.utils.JsonUtils;
import com.chiu.megalith.infra.utils.LuaScriptUtils;
import com.chiu.megalith.search.config.ElasticSearchRabbitConfig;

import lombok.RequiredArgsConstructor;

import org.springframework.amqp.rabbit.connection.CorrelationData;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

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
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());
        BlogEntity blog = blogService.findByIdAndUserId(id, userId);
        return Result.success(blog);
    }

    @PostMapping("/save")
    public Result<Void> saveOrUpdate(@RequestBody @Validated BlogEntityVo blog) {
        BlogEntity blogEntity = blogService.saveOrUpdate(blog);
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

        rabbitTemplate.convertAndSend(
                ElasticSearchRabbitConfig.ES_EXCHANGE,
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
                throw new AuthenticationExceptionImpl("must delete own blog");
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

            rabbitTemplate.convertAndSend(
                    ElasticSearchRabbitConfig.ES_EXCHANGE,
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
        throw new AuthenticationExceptionImpl("user mismatch");
    }

    @GetMapping("/blogs")
    public Result<PageAdapter<BlogEntityDto>> getAllBlogs(@RequestParam(defaultValue = "1") Integer currentPage,
                                                          @RequestParam(defaultValue = "5") Integer size) {
        PageAdapter<BlogEntityDto> page = blogService.findAllABlogs(currentPage, size);
        return Result.success(page);
    }

    @GetMapping("/deleted")
    public Result<PageAdapter<BlogEntity>> getDeletedBlogs(@RequestParam Integer currentPage,
                                                           @RequestParam Integer size) {
        PageAdapter<BlogEntity> deletedBlogs = blogService.findDeletedBlogs(currentPage, size);
        return Result.success(deletedBlogs);
    }

    @GetMapping("/recover/{id}/{idx}")
    public Result<Void> recoverDeletedBlog(@PathVariable(value = "id") Long id,
                                           @PathVariable(value = "idx") Integer idx) {
        BlogEntity blog = blogService.recoverDeletedBlog(id, idx);

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

        long userId = Long.parseLong(authentication.getName());
        Long bdUserId = blogService.findUserIdById(id);

        if (Boolean.FALSE.equals(Objects.equals(highestRole, authority) || Objects.equals(userId, bdUserId))) {
            throw new AuthenticationExceptionImpl("user unmatch");
        }

        Integer year = blogService.changeBlogStatus(id, status);
        
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

        return Result.success();
    }
}
