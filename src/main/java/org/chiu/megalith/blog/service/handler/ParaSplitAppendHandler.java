package org.chiu.megalith.blog.service.handler;

import org.chiu.megalith.blog.dto.BlogEditPushActionDto;
import org.chiu.megalith.blog.lang.PushActionEnum;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

import java.util.*;

import static org.chiu.megalith.blog.lang.MessageActionFieldEnum.VERSION;
import static org.chiu.megalith.blog.lang.PushActionEnum.PARA_SPLIT_APPEND;
import static org.chiu.megalith.infra.lang.Const.PARAGRAPH_PREFIX;

@Component
public class ParaSplitAppendHandler extends PushActionAbstractHandler {

    private final StringRedisTemplate redisTemplate;

    public ParaSplitAppendHandler(SimpMessagingTemplate simpMessagingTemplate,
                                  StringRedisTemplate redisTemplate,
                                  SimpMessagingTemplate simpMessagingTemplate1) {
        super(simpMessagingTemplate, redisTemplate);
        this.redisTemplate = redisTemplate;
        this.simpMessagingTemplate = simpMessagingTemplate1;
    }

    @Override
    public boolean match(PushActionEnum pushActionEnum) {
        return PARA_SPLIT_APPEND.equals(pushActionEnum);
    }

    @Override
    protected String getValue(String contentChange, String value, Integer indexStart, Integer indexEnd) {
        return value.substring(0, value.length() - 1);
    }

    @Override
    protected String getRedisValue(BlogEditPushActionDto dto) {
        return PARAGRAPH_PREFIX.getInfo() + (dto.getParaNo() - 1);
    }

    @Override
    protected void setContent(BlogEditPushActionDto dto, String value, Integer version) {
        Integer paraNo = dto.getParaNo();
        String redisKey = dto.getRedisKey();
        Map<String, String> subMap = new LinkedHashMap<>();
        subMap.put(PARAGRAPH_PREFIX.getInfo() + (paraNo - 1), value);
        subMap.put(PARAGRAPH_PREFIX.getInfo() + paraNo, "");
        subMap.put(VERSION.getMsg(), String.valueOf(version));
        redisTemplate.opsForHash().putAll(redisKey, subMap);
    }
}
