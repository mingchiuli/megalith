package org.chiu.megalith.blog.service.impl;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import org.chiu.megalith.blog.lang.PushActionEnum;
import org.chiu.megalith.blog.req.BlogEditPushActionReq;
import org.chiu.megalith.blog.req.BlogEditPushAllReq;
import org.chiu.megalith.blog.req.BlogEntityReq;
import org.chiu.megalith.blog.req.BlogEntityReq.BlogEntityReqBuilder;
import org.chiu.megalith.blog.service.BlogMessageService;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.utils.JsonUtils;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class BlogMessageServiceImpl implements BlogMessageService {

    private final StringRedisTemplate redisTemplate;

    private final JsonUtils jsonUtils;

    private final SimpMessagingTemplate simpMessagingTemplate;

    @Override
    public void pushAction(BlogEditPushActionReq req, Long userId) {

        Long id = req.getId();
        Integer operateTypeCode = req.getOperateTypeCode();
        PushActionEnum pushActionEnum = PushActionEnum.getInstance(operateTypeCode);
        Integer version = req.getVersion();
        Integer indexStart = req.getIndexStart();
        Integer indexEnd = req.getIndexEnd();
        String contentChange = req.getContentChange();

        String redisKey = Objects.isNull(id) ? Const.TEMP_EDIT_BLOG.getInfo() + userId : Const.TEMP_EDIT_BLOG.getInfo() + userId + ":" + id;        

        Map<String, String> entries = redisTemplate.<String, String>opsForHash().entries(redisKey);

        String v = entries.get("version");
        if (version != Integer.parseInt(v) + 1) {
            // 前端向服务端推全量
            simpMessagingTemplate.convertAndSend("/edits/push/all", "ALL");
            return;
        }

        String blogString = entries.get("blog");
        BlogEditPushAllReq blog = jsonUtils.readValue(blogString, BlogEditPushAllReq.class);
        String blogContent = blog.getContent();

        BlogEntityReqBuilder blogBuilder = BlogEntityReq.builder()
                .id(id)
                .description(blog.getDescription())
                .status(blog.getStatus())
                .link(blog.getLink())
                .title(blog.getTitle());

        switch (pushActionEnum) {
            case REMOVE -> blogBuilder.content("");
            case TAIL_APPEND -> blogBuilder.content(blogContent + contentChange);
            case TAIL_SUBTRACT -> blogBuilder.content(blogContent.substring(0, indexStart));
            case HEAD_APPEND -> blogBuilder.content(contentChange + blogContent);
            case HEAD_SUBTRACT -> blogBuilder.content(blogContent.substring(indexStart));
            case REPLACE -> blogBuilder.content(blogContent.substring(0, indexStart) + contentChange + blogContent.substring(indexEnd));
        }

        redisTemplate.execute(LuaScriptUtils.sendBlogToTempLua, Collections.singletonList(redisKey),
                "blog", "version", jsonUtils.writeValueAsString(blogBuilder.build()), String.valueOf(version), "604800");
    }

    @Override
    public void pushAll(BlogEditPushAllReq blog, Long userId) {
        Long id = blog.getId();
        String redisKey = Objects.isNull(id) ? Const.TEMP_EDIT_BLOG.getInfo() + userId : Const.TEMP_EDIT_BLOG.getInfo() + userId + ":" + id;        
        redisTemplate.execute(LuaScriptUtils.sendBlogToTempLua, Collections.singletonList(redisKey),
                "blog", "version", jsonUtils.writeValueAsString(blog), "-1", "604800");
    }

}
