package org.chiu.megalith.coop.service.impl;

import org.chiu.megalith.coop.dto.impl.FinishCoopDto;
import org.chiu.megalith.coop.vo.BlogAbstractVo;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.blog.vo.BlogExhibitVo;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.infra.utils.JsonUtils;
import org.chiu.megalith.manage.req.BlogEntityReq;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.service.UserService;
import org.chiu.megalith.coop.dto.impl.JoinCoopDto;
import org.chiu.megalith.coop.service.CoopService;
import org.chiu.megalith.coop.vo.InitCoopVo;
import org.chiu.megalith.coop.dto.UserEntityDto;

import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.util.Set;

/**
 * @author mingchiuli
 * @create 2022-12-26 1:05 am
 */
@Service
public class CoopServiceImpl extends BaseCoopService implements CoopService {

    private StringRedisTemplate redisTemplate;

    private JsonUtils jsonUtils;

    private final UserService userService;

    private BlogService blogService;

    @Value("${blog.blog-coop-size}")
    private Integer size;

    public CoopServiceImpl(StringRedisTemplate redisTemplate, JsonUtils jsonUtils, RabbitTemplate rabbitTemplate, UserService userService) {
        super(redisTemplate, jsonUtils, rabbitTemplate);
        this.userService = userService;
    }

    @Override
    public InitCoopVo initCoopSession(Long blogId, Integer orderNumber) {

        var userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        UserEntity userEntity = userService.findById(userId);
        BlogEntity blogEntity = blogService.findById(blogId);
        var userEntityVo = UserEntityDto.builder()
                .id(userEntity.getId())
                .avatar(userEntity.getAvatar())
                .nickname(userEntity.getNickname())
                .build();

        var dto = new JoinCoopDto();
        dto.setBlogId(blogId);
        dto.setFromId(userId);
        dto.setUser(userEntityVo);

        BlogEntityReq vo = BlogEntityReq.builder()
                .title(blogEntity.getTitle())
                .description(blogEntity.getDescription())
                .content(blogEntity.getContent())
                .id(blogEntity.getId())
                .status(blogEntity.getStatus())
                .link(blogEntity.getLink())
                .build();

        HashOperations<String, String, String> operations = redisTemplate.opsForHash();
        operations.put(Const.COOP_PREFIX.getInfo() + blogId,
                Const.BLOG_CONTENT.getInfo(),
                jsonUtils.writeValueAsString(vo));

        return InitCoopVo.builder()
                .blogEntity(blogEntity)
                .userEntityDto(userEntityVo)
                .build();
    }

    @Override
    public void submitBlog(Long blogId) {
        var userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        HashOperations<String, String, String> operations = redisTemplate.opsForHash();
        String contentStr = operations.get(Const.COOP_PREFIX.getInfo() + blogId, Const.BLOG_CONTENT.getInfo());
        BlogEntityReq blogVo = jsonUtils.readValue(contentStr, BlogEntityReq.class);
        blogService.saveOrUpdate(blogVo, userId);
        FinishCoopDto finishCoopDto = new FinishCoopDto();
        finishCoopDto.setFromId(userId);
        finishCoopDto.setBlogId(blogId);
        sendToOtherUsers(finishCoopDto);
        redisTemplate.delete(Const.COOP_PREFIX.getInfo() + blogId);
    }

    @Override
    public PageAdapter<BlogAbstractVo> getCoopBlogsInfo(Integer currentPage) {

        Set<String> keys = redisTemplate.keys(Const.COOP_PREFIX.getInfo() + "*");
        int total = keys.size();
        int totalPages = total % size == 0 ? total / size : total / size + 1;

        return PageAdapter.<BlogAbstractVo>builder()
                .content(keys.stream()
                        .map(key -> Long.parseLong(key.replace(Const.COOP_PREFIX.getInfo(), "")))
                        .limit((long) currentPage * size)
                        .skip((long) (currentPage - 1) * size)
                        .map(id -> {
                            BlogExhibitVo vo;
                            try {
                                vo = blogService.findById(id, true);
                            } catch (MissException e) {
                                vo = blogService.findById(id, false);
                            }
                            return BlogAbstractVo.builder()
                                    .id(id)
                                    .title(vo.getTitle())
                                    .description(vo.getDescription())
                                    .created(vo.getCreated())
                                    .build();
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
