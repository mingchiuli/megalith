package org.chiu.megalith.blog.service.handler;

import org.chiu.megalith.blog.dto.BlogEditPushActionDto;
import org.chiu.megalith.blog.lang.PushActionEnum;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

import java.util.*;

import static org.chiu.megalith.blog.lang.MessageActionFieldEnum.VERSION;
import static org.chiu.megalith.blog.lang.PushActionEnum.PARA_REPLACE;
import static org.chiu.megalith.infra.lang.Const.PARAGRAPH_PREFIX;

@Component
public class ParaReplaceHandler extends PushActionAbstractHandler {

    private final StringRedisTemplate redisTemplate;

    public ParaReplaceHandler(SimpMessagingTemplate simpMessagingTemplate,
                              StringRedisTemplate redisTemplate,
                              SimpMessagingTemplate simpMessagingTemplate1) {
        super(simpMessagingTemplate, redisTemplate);
        this.redisTemplate = redisTemplate;
        this.simpMessagingTemplate = simpMessagingTemplate1;
    }

    @Override
    public boolean match(PushActionEnum pushActionEnum) {
        return PARA_REPLACE.equals(pushActionEnum);
    }

    @Override
    protected String getValue(String contentChange, String value, Integer indexStart, Integer indexEnd) {
        return value.substring(0, indexStart) + contentChange + value.substring(indexEnd);
    }

    @Override
    protected String getRedisValue(BlogEditPushActionDto dto) {
        return dto.getParaNo().toString();
    }

    @Override
    protected void setContent(BlogEditPushActionDto dto, String value, Integer version) {
        Integer paraNo = dto.getParaNo();
        String redisKey = dto.getRedisKey();
        Map<String, String> subMap = new LinkedHashMap<>();
        subMap.put(PARAGRAPH_PREFIX.getInfo() + paraNo, value);
        subMap.put(VERSION.getMsg(), version.toString());
        redisTemplate.opsForHash().putAll(redisKey, subMap);
    }
}
