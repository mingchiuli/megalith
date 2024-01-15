package org.chiu.megalith.blog.service.impl;

import java.util.*;

import lombok.SneakyThrows;
import org.chiu.megalith.blog.lang.FieldEnum;
import org.chiu.megalith.blog.lang.ParaOpreateEnum;
import org.chiu.megalith.blog.lang.PushActionEnum;
import org.chiu.megalith.blog.req.BlogEditPushActionReq;
import org.chiu.megalith.blog.req.BlogEditPushAllReq;
import org.chiu.megalith.blog.service.BlogMessageService;
import org.chiu.megalith.infra.utils.JsonUtils;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;

import static org.chiu.megalith.infra.lang.Const.*;
import static org.chiu.megalith.blog.lang.MessageActionFieldEnum.*;


@Service
@RequiredArgsConstructor
public class BlogMessageServiceImpl implements BlogMessageService {

    private final StringRedisTemplate redisTemplate;

    private final SimpMessagingTemplate simpMessagingTemplate;

    private final JsonUtils jsonUtils;

    @SneakyThrows
    @Override
    @SuppressWarnings("unchecked")
    public void pushAction(BlogEditPushActionReq req, Long userId) {

        Long id = req.getId();
        Integer paraNo = req.getParaNo();
        Integer paraTypeCode = req.getParaTypeCode();
        Integer operateTypeCode = req.getOperateTypeCode();
        ParaOpreateEnum paraOpreateEnum = null;
        if (Objects.nonNull(paraTypeCode)) {
            paraOpreateEnum = ParaOpreateEnum.getInstance(paraTypeCode);
        }
        PushActionEnum pushActionEnum = PushActionEnum.getInstance(operateTypeCode);
        Integer version = req.getVersion();
        Integer indexStart = req.getIndexStart();
        Integer indexEnd = req.getIndexEnd();
        String contentChange = req.getContentChange();
        String fieldName = req.getField();
        FieldEnum fieldEnum = FieldEnum.getInstance(fieldName);

        String redisKey = Objects.isNull(id) ?
                TEMP_EDIT_BLOG.getInfo() + userId :
                TEMP_EDIT_BLOG.getInfo() + userId + ":" + id;

        String v;
        String value;
        if (Objects.nonNull(paraOpreateEnum)) {
            //content字段
            switch (paraOpreateEnum) {
                case NONE -> {
                    List<String> resp =  Optional.ofNullable((redisTemplate.execute(LuaScriptUtils.hGetTwoArgs,
                                    Collections.singletonList(redisKey),
                                    VERSION.getMsg(), PARAGRAPH_PREFIX.getInfo() + paraNo)))
                            .orElseGet(ArrayList::new);

                    v = resp.getFirst();
                    value = resp.getLast();
                    value = Objects.isNull(value) ? "" : value;

                    if (version != Integer.parseInt(v) + 1) {
                        // 前端向服务端推全量
                        simpMessagingTemplate.convertAndSend("/edits/push/all", "ALL");
                        return;
                    }

                    switch (pushActionEnum) {
                        case REMOVE -> value = "";
                        case TAIL_APPEND -> value = value + contentChange;
                        case TAIL_SUBTRACT -> value = value.substring(0, indexStart);
                        case HEAD_APPEND -> value = contentChange + value;
                        case HEAD_SUBTRACT -> value = value.substring(indexStart);
                        case REPLACE -> value = value.substring(0, indexStart) + contentChange + value.substring(indexEnd);
                        default -> throw new IllegalArgumentException("Unexpected value: " + pushActionEnum);
                    }
                    Map<String, String> subMap = new LinkedHashMap<>();
                    subMap.put(PARAGRAPH_PREFIX.getInfo() + paraNo, value);
                    subMap.put(VERSION.getMsg(), version.toString());
                    redisTemplate.opsForHash().putAll(redisKey, subMap);
                    return;
                }

                case TAIL_APPEND -> {
                    List<String> resp =  Optional.ofNullable((redisTemplate.execute(LuaScriptUtils.hGetTwoArgs,
                                    Collections.singletonList(redisKey),
                                    VERSION.getMsg(), PARAGRAPH_PREFIX.getInfo() + (paraNo - 1))))
                            .orElseGet(ArrayList::new);
                    v = resp.getFirst();
                    if (version != Integer.parseInt(v) + 1) {
                        // 前端向服务端推全量
                        simpMessagingTemplate.convertAndSend("/edits/push/all", "ALL");
                        return;
                    }
                    value = resp.getLast();
                    //去掉最后的\n
                    value = value.substring(0, value.length() - 1);
                    Map<String, String> subMap = new LinkedHashMap<>();
                    subMap.put(PARAGRAPH_PREFIX.getInfo() + (paraNo - 1), value);
                    subMap.put(PARAGRAPH_PREFIX.getInfo() + paraNo, "");
                    subMap.put(VERSION.getMsg(), String.valueOf(version));
                    redisTemplate.opsForHash().putAll(redisKey, subMap);
                    return;
                }

                case TAIL_SUBTRACT -> {
                    List<String> resp =  Optional.ofNullable((redisTemplate.execute(LuaScriptUtils.hGetTwoArgs,
                                    Collections.singletonList(redisKey),
                                    VERSION.getMsg(), PARAGRAPH_PREFIX.getInfo() + (paraNo - 1))))
                            .orElseGet(ArrayList::new);
                    v = resp.getFirst();
                    if (version != Integer.parseInt(v) + 1) {
                        // 前端向服务端推全量
                        simpMessagingTemplate.convertAndSend("/edits/push/all", "ALL");
                        return;
                    }

                    value = resp.getLast();
                    value = value + '\n';

                    redisTemplate.execute(LuaScriptUtils.tailSubtractContentLua, Collections.singletonList(redisKey),
                            PARAGRAPH_PREFIX.getInfo() + paraNo, PARAGRAPH_PREFIX.getInfo() +(paraNo - 1), value, VERSION.getMsg(), String.valueOf(version));
                    return;
                }
            }
        }
        //其他字段
        List<String> resp =  Optional.ofNullable((redisTemplate.execute(LuaScriptUtils.hGetTwoArgs,
                        Collections.singletonList(redisKey),
                        VERSION.getMsg(), fieldEnum.getField())))
                .orElseGet(ArrayList::new);
        v = resp.getFirst();
        value = resp.getLast();

        if (version != Integer.parseInt(v) + 1) {
            // 前端向服务端推全量
            simpMessagingTemplate.convertAndSend("/edits/push/all", "ALL");
            return;
        }

        switch (pushActionEnum) {
            case REMOVE -> value = "";
            case TAIL_APPEND -> value = value + contentChange;
            case TAIL_SUBTRACT -> value = value.substring(0, indexStart);
            case HEAD_APPEND -> value = contentChange + value;
            case HEAD_SUBTRACT -> value = value.substring(indexStart);
            case REPLACE -> value = value.substring(0, indexStart) + contentChange + value.substring(indexEnd);
            case NONE -> value = contentChange;
            default -> throw new IllegalArgumentException("Unexpected value: " + pushActionEnum);
        }

        redisTemplate.execute(LuaScriptUtils.pushActionLua, Collections.singletonList(redisKey),
                fieldEnum.getField(), VERSION.getMsg(),
                value, String.valueOf(version),
                "604800");
    }

    @Override
    public void pushAll(BlogEditPushAllReq blog, Long userId) {
        Long id = blog.getId();
        String redisKey = Objects.isNull(id) ?
                TEMP_EDIT_BLOG.getInfo() + userId :
                TEMP_EDIT_BLOG.getInfo() + userId + ":" + id;

        String content = blog.getContent();

        List<String> paragraphList = List.of(content.split(PARAGRAPH_SPLITTER.getInfo()));
        String paragraphListString = jsonUtils.writeValueAsString(paragraphList);

        redisTemplate.execute(LuaScriptUtils.pushAllLua, Collections.singletonList(redisKey),
                paragraphListString, ID.getMsg(), TITLE.getMsg(), DESCRIPTION.getMsg(), STATUS.getMsg(), LINK.getMsg(), VERSION.getMsg(),
                Objects.isNull(blog.getId()) ? "" : blog.getId().toString(), blog.getTitle(), blog.getDescription(), blog.getStatus().toString(), blog.getLink(), "-1",
                "604800");
    }

}
