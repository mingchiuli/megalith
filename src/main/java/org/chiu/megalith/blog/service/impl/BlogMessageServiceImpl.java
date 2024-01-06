package org.chiu.megalith.blog.service.impl;

import java.util.Collections;
import java.util.Objects;

import lombok.SneakyThrows;
import org.chiu.megalith.blog.lang.FieldEnum;
import org.chiu.megalith.blog.lang.PushActionEnum;
import org.chiu.megalith.blog.req.BlogEditPushActionReq;
import org.chiu.megalith.blog.req.BlogEditPushAllReq;
import org.chiu.megalith.blog.service.BlogMessageService;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class BlogMessageServiceImpl implements BlogMessageService {

    private final StringRedisTemplate redisTemplate;

    private final SimpMessagingTemplate simpMessagingTemplate;

    @SneakyThrows
    @Override
    public void pushAction(BlogEditPushActionReq req, Long userId) {

        Long id = req.getId();
        Integer operateTypeCode = req.getOperateTypeCode();
        PushActionEnum pushActionEnum = PushActionEnum.getInstance(operateTypeCode);
        Integer version = req.getVersion();
        Integer indexStart = req.getIndexStart();
        Integer indexEnd = req.getIndexEnd();
        String contentChange = req.getContentChange();
        String fieldName = req.getField();
        FieldEnum fieldEnum = FieldEnum.getInstance(fieldName);

        String redisKey = Objects.isNull(id) ?
                Const.TEMP_EDIT_BLOG.getInfo() + userId :
                Const.TEMP_EDIT_BLOG.getInfo() + userId + ":" + id;

        String v = redisTemplate.<String, String>opsForHash().get(redisKey, "version");
        if (version != Integer.parseInt(v) + 1) {
            // 前端向服务端推全量
            simpMessagingTemplate.convertAndSend("/edits/push/all", "ALL");
            return;
        }

        String fieldValue = redisTemplate.<String, String>opsForHash().get(redisKey, fieldEnum.getField());

        switch (pushActionEnum) {
            case REMOVE -> fieldValue = "";
            case TAIL_APPEND -> fieldValue = fieldValue + contentChange;
            case TAIL_SUBTRACT -> fieldValue = fieldValue.substring(0, indexStart);
            case HEAD_APPEND -> fieldValue = contentChange + fieldValue;
            case HEAD_SUBTRACT -> fieldValue = fieldValue.substring(indexStart);
            case REPLACE -> fieldValue = fieldValue.substring(0, indexStart) + contentChange + fieldValue.substring(indexEnd);
            case NONE -> fieldValue = contentChange;//status变更
        }

        redisTemplate.execute(LuaScriptUtils.pushActionLua, Collections.singletonList(redisKey),
                fieldEnum.getField(), "version",
                fieldValue, String.valueOf(version),
                "604800");
    }

    @Override
    public void pushAll(BlogEditPushAllReq blog, Long userId) {
        Long id = blog.getId();
        String redisKey = Objects.isNull(id) ?
                Const.TEMP_EDIT_BLOG.getInfo() + userId :
                Const.TEMP_EDIT_BLOG.getInfo() + userId + ":" + id;
        redisTemplate.execute(LuaScriptUtils.pushAllLua, Collections.singletonList(redisKey),
                "id", "title", "description", "content", "status", "link", "version",
                Objects.isNull(blog.getId()) ? "" : blog.getId().toString(), blog.getTitle(), blog.getDescription(), blog.getContent(), blog.getStatus().toString(), blog.getLink(), "-1", "604800");
    }

}
