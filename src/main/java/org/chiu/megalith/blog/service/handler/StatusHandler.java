package org.chiu.megalith.blog.service.handler;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.blog.dto.BlogEditPushActionDto;
import org.chiu.megalith.blog.lang.FieldEnum;
import org.chiu.megalith.blog.lang.PushActionEnum;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.chiu.megalith.blog.lang.MessageActionFieldEnum.VERSION;
import static org.chiu.megalith.blog.lang.PushActionEnum.STATUS;

@Component
@RequiredArgsConstructor
public class StatusHandler extends PushActionAbstractHandler {

    private final StringRedisTemplate redisTemplate;

    private final SimpMessagingTemplate simpMessagingTemplate;

    @Override
    public boolean match(PushActionEnum pushActionEnum) {
        return STATUS.equals(pushActionEnum);
    }

    @Override
    @SuppressWarnings("unchecked")
    public void handle(BlogEditPushActionDto dto) {

        String redisKey = dto.getRedisKey();
        FieldEnum fieldEnum = dto.getFieldEnum();
        String userKey = dto.getUserKey();
        String contentChange = dto.getContentChange();
        Integer version = dto.getVersion();

        List<String> resp =  Optional.ofNullable((redisTemplate.execute(LuaScriptUtils.hGetTwoArgs,
                        Collections.singletonList(redisKey),
                        VERSION.getMsg(), fieldEnum.getField())))
                .orElseGet(ArrayList::new);
        String v = resp.getFirst();
        String value;
        int rawVersion = Integer.parseInt(v);
        int newVersion = dto.getVersion();

        if (newVersion != rawVersion + 1) {
            // 前端向服务端推全量
            simpMessagingTemplate.convertAndSend("/edits/push/all/" + userKey, "ALL");
            return;
        }
        value = contentChange;

        redisTemplate.execute(LuaScriptUtils.pushActionLua, Collections.singletonList(redisKey),
                fieldEnum.getField(), VERSION.getMsg(),
                value, String.valueOf(version),
                "604800");
    }
}
