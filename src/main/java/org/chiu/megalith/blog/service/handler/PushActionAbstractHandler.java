package org.chiu.megalith.blog.service.handler;

import org.chiu.megalith.blog.dto.BlogEditPushActionDto;
import org.chiu.megalith.blog.lang.PushActionEnum;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.simp.SimpMessagingTemplate;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.chiu.megalith.blog.lang.MessageActionFieldEnum.VERSION;
import static org.chiu.megalith.infra.lang.Const.PARAGRAPH_PREFIX;

public abstract class PushActionAbstractHandler {

    protected SimpMessagingTemplate simpMessagingTemplate;

    protected StringRedisTemplate redisTemplate;

    public PushActionAbstractHandler(SimpMessagingTemplate simpMessagingTemplate,
                                     StringRedisTemplate redisTemplate) {
        this.simpMessagingTemplate = simpMessagingTemplate;
        this.redisTemplate = redisTemplate;
    }

    public abstract boolean match(PushActionEnum pushActionEnum);

    @SuppressWarnings("unchecked")
    public void handle(BlogEditPushActionDto dto) {
        String redisKey = dto.getRedisKey();
        String userKey = dto.getUserKey();
        String contentChange = dto.getContentChange();
        Integer indexStart = dto.getIndexStart();
        Integer indexEnd = dto.getIndexEnd();

        String redisValue = getRedisValue(dto);

        List<String> rawContent = Optional.ofNullable((redisTemplate.execute(LuaScriptUtils.hGetTwoArgs,
                        Collections.singletonList(redisKey),
                        VERSION.getMsg(), PARAGRAPH_PREFIX.getInfo() + redisValue)))
                .orElseGet(ArrayList::new);

        String v = rawContent.getFirst();
        String value = rawContent.getLast();
        int rawVersion = Integer.parseInt(v);
        int newVersion = dto.getVersion();

        if (newVersion != rawVersion + 1) {
            // 前端向服务端推全量
            simpMessagingTemplate.convertAndSend("/edits/push/all/" + userKey, "ALL");
            return;
        }

        String newValue = getValue(contentChange, value, indexStart, indexEnd);
        setContent(dto, newValue, newVersion);
    }

    protected abstract String getValue(String contentChange, String value, Integer indexStart, Integer indexEnd);

    protected abstract String getRedisValue(BlogEditPushActionDto dto);

    protected abstract void setContent(BlogEditPushActionDto dto, String value, Integer version);

}
