package org.chiu.megalith.websocket.service.impl;

import jakarta.annotation.PostConstruct;
import lombok.SneakyThrows;
import org.chiu.megalith.infra.utils.JsonUtils;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.repository.BlogRepository;
import org.chiu.megalith.websocket.convertor.BlogEditVoConvertor;
import org.chiu.megalith.websocket.convertor.BlogEntityConvertor;
import org.chiu.megalith.websocket.req.BlogEditPushActionReq;
import org.chiu.megalith.websocket.req.BlogEditPushAllReq;
import org.chiu.megalith.websocket.service.BlogMessageService;
import org.chiu.megalith.websocket.vo.BlogEditVo;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.key.KeyFactory;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import org.springframework.util.ResourceUtils;
import java.nio.charset.StandardCharsets;
import java.util.*;

import static org.chiu.megalith.infra.lang.Const.*;
import static org.chiu.megalith.websocket.lang.MessageActionFieldEnum.*;
import static org.chiu.megalith.infra.lang.ExceptionMessage.*;

@Service
@RequiredArgsConstructor
public class BlogMessageServiceImpl implements BlogMessageService {

    private final SimpMessagingTemplate simpMessagingTemplate;

    private final StringRedisTemplate redisTemplate;

    private final ResourceLoader resourceLoader;

    private final JsonUtils jsonUtils;

    private final BlogRepository blogRepository;

    private String pushActionScript;

    private String pushAllScript;

    @PostConstruct
    @SneakyThrows
    private void init() {
        Resource pushActionResource = resourceLoader.getResource(ResourceUtils.CLASSPATH_URL_PREFIX + "script/push-action.lua");
        pushActionScript = pushActionResource.getContentAsString(StandardCharsets.UTF_8);
        Resource pushAllResource = resourceLoader.getResource(ResourceUtils.CLASSPATH_URL_PREFIX + "script/push-all.lua");
        pushAllScript = pushAllResource.getContentAsString(StandardCharsets.UTF_8);
    }

    @Override
    public void pushAction(BlogEditPushActionReq req, Long userId) {
        Long blogId = req.getId();
        String contentChange = req.getContentChange();
        Integer operateTypeCode = req.getOperateTypeCode();
        Integer version = req.getVersion();
        Integer indexStart = req.getIndexStart();
        Integer indexEnd = req.getIndexEnd();
        String field = req.getField();
        Integer paraNo = req.getParaNo();

        String userKey = KeyFactory.createPushContentIdentityKey(userId, blogId);

        String redisKey = KeyFactory.createBlogEditRedisKey(userId, blogId);

        Long execute = redisTemplate.execute(RedisScript.of(pushActionScript, Long.class), Collections.singletonList(redisKey),
                contentChange,
                operateTypeCode.toString(),
                version.toString(),
                Objects.nonNull(indexStart) ? indexStart.toString() : null,
                Objects.nonNull(indexEnd) ? indexEnd.toString() : null,
                Objects.nonNull(field) ? field : null,
                Objects.nonNull(paraNo) ? paraNo.toString() : null);

        if (Long.valueOf(-1).equals(execute)) {
            simpMessagingTemplate.convertAndSend("/edits/push/" + userKey, version.toString());
        }

        if (Long.valueOf(-2).equals(execute)) {
            simpMessagingTemplate.convertAndSend("/edits/pull/" + userKey, version.toString());
        }
    }

    @Override
    public void pushAll(BlogEditPushAllReq blog, Long userId) {
        Long id = blog.getId();
        String redisKey = Objects.isNull(id) ? TEMP_EDIT_BLOG.getInfo() + userId
                : TEMP_EDIT_BLOG.getInfo() + userId + ":" + id;

        String content = blog.getContent();

        List<String> paragraphList = List.of(content.split(PARAGRAPH_SPLITTER.getInfo()));
        String paragraphListString = jsonUtils.writeValueAsString(paragraphList);

        redisTemplate.execute(RedisScript.of(pushAllScript),
                Collections.singletonList(redisKey),
                paragraphListString, ID.getMsg(), USER_ID.getMsg(), TITLE.getMsg(), DESCRIPTION.getMsg(),
                STATUS.getMsg(), LINK.getMsg(), VERSION.getMsg(),
                Objects.isNull(blog.getId()) ? "" : blog.getId().toString(), userId.toString(), blog.getTitle(),
                blog.getDescription(), blog.getStatus().toString(), blog.getLink(), blog.getVersion().toString(),
                A_WEEK.getInfo());
    }

    @Override
    public BlogEditVo findEdit(Long id, Long userId) {

        String redisKey = KeyFactory.createBlogEditRedisKey(userId, id);
        Map<String, String> entries = redisTemplate.<String, String>opsForHash()
                .entries(redisKey);

        BlogEntity blog;
        int version = -1;
        String paragraphListString;
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

            List<String> paragraphList = List.of(blog.getContent().split(PARAGRAPH_SPLITTER.getInfo()));
            paragraphListString = jsonUtils.writeValueAsString(paragraphList);
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
            paragraphListString = "[]";
        } else {
            blog = blogRepository.findByIdAndUserId(id, userId)
                    .orElseThrow(() -> new MissException(EDIT_NO_AUTH));
            List<String> paragraphList = List.of(blog.getContent().split(PARAGRAPH_SPLITTER.getInfo()));
            paragraphListString = jsonUtils.writeValueAsString(paragraphList);
        }

        redisTemplate.execute(RedisScript.of(pushAllScript),
                Collections.singletonList(redisKey),
                paragraphListString, ID.getMsg(), USER_ID.getMsg(), TITLE.getMsg(), DESCRIPTION.getMsg(),
                STATUS.getMsg(), LINK.getMsg(), VERSION.getMsg(),
                    Objects.isNull(blog.getId()) ? "" : blog.getId().toString(), userId.toString(), blog.getTitle(),
                blog.getDescription(), blog.getStatus().toString(), blog.getLink(), Integer.toString(version),
                A_WEEK.getInfo());

        return BlogEditVoConvertor.convert(blog, version);
    }

}
