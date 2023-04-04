package com.chiu.megalith.coop.service.impl;

import com.chiu.megalith.coop.dto.MessageDto;
import com.chiu.megalith.coop.vo.BlogAbstractVo;
import com.chiu.megalith.exhibit.entity.BlogEntity;
import com.chiu.megalith.exhibit.service.BlogService;
import com.chiu.megalith.exhibit.vo.BlogExhibitVo;
import com.chiu.megalith.infra.exception.NotFoundException;
import com.chiu.megalith.infra.page.PageAdapter;
import com.chiu.megalith.manage.vo.BlogEntityVo;
import com.chiu.megalith.infra.lang.Const;
import com.chiu.megalith.infra.utils.JsonUtils;
import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.service.UserService;
import com.chiu.megalith.coop.config.CoopRabbitConfig;
import com.chiu.megalith.coop.dto.impl.SubmitBlogDto;
import com.chiu.megalith.coop.dto.impl.JoinBlogDto;
import com.chiu.megalith.coop.service.CoopService;
import com.chiu.megalith.coop.vo.InitCoopVo;
import com.chiu.megalith.coop.vo.UserEntityVo;
import lombok.RequiredArgsConstructor;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

/**
 * @author mingchiuli
 * @create 2022-12-26 1:05 am
 */
@Service
@RequiredArgsConstructor
public class CoopServiceImpl implements CoopService {

    private final UserService userService;

    private final StringRedisTemplate redisTemplate;

    private final RabbitTemplate rabbitTemplate;

    private final BlogService blogService;

    private final JsonUtils jsonUtils;

    @Value("${blog.blog-coop-size}")
    private Integer size;

    @Override
    public InitCoopVo joinCoopBlog(Long blogId,
                               Integer orderNumber) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        UserEntity userEntity = userService.findById(userId);
        BlogEntity blogEntity = blogService.findById(blogId);
        UserEntityVo userEntityVo = UserEntityVo.builder()
                .id(userEntity.getId())
                .avatar(userEntity.getAvatar())
                .nickname(userEntity.getNickname())
                .build();

        JoinBlogDto.Bind bind = JoinBlogDto.Bind.builder()
                .blogId(blogId)
                .fromId(userId)
                .user(userEntityVo)
                .build();

        JoinBlogDto dto = JoinBlogDto.builder()
                .content(new MessageDto.Container<>(bind))
                .build();

        List<String> usersStr = redisTemplate.opsForList().range(Const.COOP_PREFIX.getInfo() + blogId, 0, -1);

        usersStr.stream()
                .map(str -> jsonUtils.readValue(str, UserEntityVo.class))
                .filter(user -> !user.getId().equals(userId))
                .forEach(user -> {
                    bind.setToId(user.getId());
                    rabbitTemplate.convertAndSend(
                            CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                            CoopRabbitConfig.WS_BINDING_KEY + user.getNodeMark(),
                            dto);
                });

        return InitCoopVo.builder()
                .blogEntity(blogEntity)
                .userEntityVos(usersStr)
                .build();
    }

    @Override
    public void submitBlog(Long blogId,
                           BlogEntityVo blogEntityVo) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        blogService.saveOrUpdate(blogEntityVo);

        SubmitBlogDto.Bind bind = SubmitBlogDto.Bind.builder()
                .blogId(blogId)
                .fromId(userId)
                .build();

        SubmitBlogDto dto = SubmitBlogDto.builder()
                .content(new MessageDto.Container<>(bind))
                .build();

        redisTemplate.opsForList().range(Const.COOP_PREFIX.getInfo() + blogId, 0, -1).stream()
                .map(str -> jsonUtils.readValue(str, UserEntityVo.class))
                .filter(user -> userId != user.getId())
                .forEach(user -> {
                    bind.setToId(user.getId());
                    rabbitTemplate.convertAndSend(
                            CoopRabbitConfig.WS_TOPIC_EXCHANGE,
                            CoopRabbitConfig.WS_BINDING_KEY + user.getNodeMark(),
                            dto);
                });

        redisTemplate.delete(Const.COOP_PREFIX.getInfo() + blogId);
    }

    @Override
    public PageAdapter<BlogAbstractVo> getCoopBlogs(Integer currentPage) {

        Set<String> keys = redisTemplate.keys(Const.COOP_PREFIX.getInfo() + "*");
        int total = keys.size();
        int totalPages = total % size == 0 ? total / size : total / size + 1;

        return PageAdapter.<BlogAbstractVo>builder()
                .content(keys.stream()
                        .map(key -> Long.valueOf(key.replace(Const.COOP_PREFIX.getInfo(), "")))
                        .limit((long) currentPage * size)
                        .skip((long) (currentPage - 1) * size)
                        .map(id -> {
                            try {
                                BlogExhibitVo vo = blogService.findByIdAndVisible(id);
                                return BlogAbstractVo.builder()
                                        .id(id)
                                        .title(vo.getTitle())
                                        .description(vo.getDescription())
                                        .created(vo.getCreated())
                                        .build();
                            } catch (NotFoundException e) {
                                return blogService.findAbstractById(id);
                            }
                        })
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
}
