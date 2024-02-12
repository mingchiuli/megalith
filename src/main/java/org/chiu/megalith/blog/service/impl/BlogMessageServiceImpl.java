package org.chiu.megalith.blog.service.impl;

import java.util.*;

import lombok.SneakyThrows;
import org.chiu.megalith.blog.lang.FieldEnum;
import org.chiu.megalith.blog.lang.ParaOpreateEnum;
import org.chiu.megalith.blog.lang.PushActionEnum;
import org.chiu.megalith.blog.req.BlogEditPushActionReq;
import org.chiu.megalith.blog.service.BlogMessageService;
import org.chiu.megalith.infra.key.KeyFactory;
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

        String redisKey = KeyFactory.createBlogEditRedisKey(userId, id);

        String v;
        String value;
        if (Objects.nonNull(paraOpreateEnum)) {
            //content字段
            switch (paraOpreateEnum) {
                case INNER -> {
                    List<String> resp =  Optional.ofNullable((redisTemplate.execute(LuaScriptUtils.hGetTwoArgs,
                                    Collections.singletonList(redisKey),
                                    VERSION.getMsg(), PARAGRAPH_PREFIX.getInfo() + paraNo)))
                            .orElseGet(ArrayList::new);

                    v = resp.getFirst();
                    value = resp.getLast();
                    value = Objects.isNull(value) ? "" : value;

                    checkVersion(version, Integer.parseInt(v), userId, id);
                    value = contentDeal(pushActionEnum, value, contentChange, indexStart, indexEnd, userId, id);

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
                    checkVersion(version, Integer.parseInt(v), userId, id);

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
                    checkVersion(version, Integer.parseInt(v), userId, id);

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
        checkVersion(version, Integer.parseInt(v), userId, id);

        value = contentDeal(pushActionEnum, value, contentChange, indexStart, indexEnd, userId, id);

        redisTemplate.execute(LuaScriptUtils.pushActionLua, Collections.singletonList(redisKey),
                fieldEnum.getField(), VERSION.getMsg(),
                value, String.valueOf(version),
                "604800");
    }

    private void checkVersion(int newVersion, int rawVersion, Long userId, Long blogId) {
        String key = KeyFactory.createPushContentIdentityKey(userId, blogId);
        if (newVersion != rawVersion + 1) {
            // 前端向服务端推全量
            simpMessagingTemplate.convertAndSend("/edits/push/all/" + key, "ALL");
        }
    }

    private String contentDeal(PushActionEnum pushActionEnum, String rawContent, String contentChange, Integer indexStart, Integer indexEnd, Long userId, Long blogId) {
        try {
            switch (pushActionEnum) {
                case REMOVE -> rawContent = "";
                case TAIL_APPEND -> rawContent = rawContent + contentChange;
                case TAIL_SUBTRACT -> rawContent = rawContent.substring(0, indexStart);
                case HEAD_APPEND -> rawContent = contentChange + rawContent;
                case HEAD_SUBTRACT -> rawContent = rawContent.substring(indexStart);
                case REPLACE -> rawContent = rawContent.substring(0, indexStart) + contentChange + rawContent.substring(indexEnd);
                case NONE -> rawContent = contentChange;
                default -> throw new IllegalArgumentException("Unexpected value: " + pushActionEnum);
            }
        } catch (IndexOutOfBoundsException e) {
            String key = KeyFactory.createPushContentIdentityKey(userId, blogId);
            simpMessagingTemplate.convertAndSend("/edits/push/all/" + key, "ALL");
            throw e;
        }
        return rawContent;
    }

}
