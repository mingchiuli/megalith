package org.chiu.megalith.blog.service.handler;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.blog.dto.BlogEditPushActionDto;
import org.chiu.megalith.blog.lang.PushActionEnum;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

import java.util.*;

import static org.chiu.megalith.blog.lang.MessageActionFieldEnum.VERSION;
import static org.chiu.megalith.blog.lang.PushActionEnum.PARA_SPLIT_APPEND;
import static org.chiu.megalith.infra.lang.Const.PARAGRAPH_PREFIX;

@Component
@RequiredArgsConstructor
public class ParaSplitAppendHandler extends PushActionAbstractHandler {

    private final StringRedisTemplate redisTemplate;

    private final SimpMessagingTemplate simpMessagingTemplate;

    @Override
    public boolean match(PushActionEnum pushActionEnum) {
        return PARA_SPLIT_APPEND.equals(pushActionEnum);
    }

    @Override
    @SuppressWarnings("unchecked")
    public void handle(BlogEditPushActionDto dto) {
        String redisKey = dto.getRedisKey();
        String userKey = dto.getUserKey();
        Integer version = dto.getVersion();
        Integer paraNo = dto.getParaNo();

        List<String> resp =  Optional.ofNullable((redisTemplate.execute(LuaScriptUtils.hGetTwoArgs,
                        Collections.singletonList(redisKey),
                        VERSION.getMsg(), PARAGRAPH_PREFIX.getInfo() + (paraNo - 1))))
                .orElseGet(ArrayList::new);
        String v = resp.getFirst();
        String value = resp.getLast();
        value = value.substring(0, value.length() - 1);

        int rawVersion = Integer.parseInt(v);
        int newVersion = dto.getVersion();

        if (newVersion != rawVersion + 1) {
            // 前端向服务端推全量
            simpMessagingTemplate.convertAndSend("/edits/push/all/" + userKey, "ALL");
            return;
        }

        Map<String, String> subMap = new LinkedHashMap<>();
        subMap.put(PARAGRAPH_PREFIX.getInfo() + (paraNo - 1), value);
        subMap.put(PARAGRAPH_PREFIX.getInfo() + paraNo, "");
        subMap.put(VERSION.getMsg(), String.valueOf(version));
        redisTemplate.opsForHash().putAll(redisKey, subMap);
    }
}
