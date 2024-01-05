package org.chiu.megalith.blog.service.impl;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import lombok.SneakyThrows;
import org.chiu.megalith.blog.dto.BLogEntityDto;
import org.chiu.megalith.blog.lang.FieldEnum;
import org.chiu.megalith.blog.lang.PushActionEnum;
import org.chiu.megalith.blog.req.BlogEditPushActionReq;
import org.chiu.megalith.blog.req.BlogEditPushAllReq;
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

        String redisKey = Objects.isNull(id) ? Const.TEMP_EDIT_BLOG.getInfo() + userId : Const.TEMP_EDIT_BLOG.getInfo() + userId + ":" + id;        

        Map<String, String> entries = redisTemplate.<String, String>opsForHash().entries(redisKey);

        String v = entries.get("version");
        if (version != Integer.parseInt(v) + 1) {
            // 前端向服务端推全量
            simpMessagingTemplate.convertAndSend("/edits/push/all", "ALL");
            return;
        }

        String blogString = entries.get("blog");
        BLogEntityDto blog = jsonUtils.readValue(blogString, BLogEntityDto.class);

        BLogEntityDto dto = new BLogEntityDto();
        for (Field field : blog.getClass().getDeclaredFields()) {
            Object fieldValue;
            field.setAccessible(true);
            String name = field.getName();

            if (fieldEnum.getField().equals("status") && name.equals("status")) {
                field.set(dto, Integer.valueOf(contentChange));
            } else if (fieldEnum.getField().equals(name)) {
                fieldValue = field.get(blog);
                switch (pushActionEnum) {
                    case REMOVE -> field.set(dto, "");
                    case TAIL_APPEND -> field.set(dto, fieldValue.toString() + contentChange);
                    case TAIL_SUBTRACT -> field.set(dto, fieldValue.toString().substring(0, indexStart));
                    case HEAD_APPEND -> field.set(dto, contentChange + fieldValue.toString());
                    case HEAD_SUBTRACT -> field.set(dto, fieldValue.toString().substring(indexStart));
                    case REPLACE -> field.set(dto, fieldValue.toString().substring(0, indexStart) + contentChange + fieldValue.toString().substring(indexEnd));
                }
            } else {
                fieldValue = field.get(blog);
                field.set(dto, fieldValue);
            }
        }

        redisTemplate.execute(LuaScriptUtils.sendBlogToTempLua, Collections.singletonList(redisKey),
                "blog", "version", jsonUtils.writeValueAsString(dto), String.valueOf(version), "604800");

    }

    @Override
    public void pushAll(BlogEditPushAllReq blog, Long userId) {
        Long id = blog.getId();
        String redisKey = Objects.isNull(id) ? Const.TEMP_EDIT_BLOG.getInfo() + userId : Const.TEMP_EDIT_BLOG.getInfo() + userId + ":" + id;        
        redisTemplate.execute(LuaScriptUtils.sendBlogToTempLua, Collections.singletonList(redisKey),
                "blog", "version", jsonUtils.writeValueAsString(blog), "-1", "604800");
    }

}
