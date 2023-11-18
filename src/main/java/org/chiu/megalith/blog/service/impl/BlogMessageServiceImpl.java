package org.chiu.megalith.blog.service.impl;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import org.chiu.megalith.blog.lang.PushActionEnum;
import org.chiu.megalith.blog.req.BlogEditPushActionReq;
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
        Integer version = req.getVersion();
        BlogEntityReqBuilder blogBuilder = BlogEntityReq.builder()
                .description(req.getDescription())
                .link(req.getLink())
                .status(req.getStatus())
                .title(req.getTitle())
                .id(id);

        String redisKey = Objects.isNull(id) ? Const.TEMP_EDIT_BLOG.getInfo() + userId : Const.TEMP_EDIT_BLOG.getInfo() + userId + ":" + id;        

        Map<String, String> entries = redisTemplate.<String, String>opsForHash().entries(redisKey);

        String v = entries.get("version");
        if (version != Integer.parseInt(v) + 1) {
            // 前端向服务端推全量
            simpMessagingTemplate.convertAndSend("/edits/push/all", "ALL");
            return;
        }

        String blogString = entries.get("blog");
        BlogEntityReq blog = jsonUtils.readValue(blogString, BlogEntityReq.class);
        String blogContent = blog.getContent();

        String contentChange = req.getContentChange();
        if (PushActionEnum.APPEND.getCode().equals(operateTypeCode)) {
            blogBuilder.content(blogContent + contentChange);
        } else if (PushActionEnum.SUBSTRACT.getCode().equals(operateTypeCode)) {
            blogBuilder.content(blogContent.substring(0, blogContent.length() - contentChange.length()));
        } else {
            // 前端向服务端推全量
            simpMessagingTemplate.convertAndSend("/edits/push/all", "ALL");
            return;
        }

        redisTemplate.execute(LuaScriptUtils.sendBlogToTempLua, Collections.singletonList(redisKey),
                "blog", "version", jsonUtils.writeValueAsString(blogBuilder.build()), String.valueOf(version), "604800");
    }

}
