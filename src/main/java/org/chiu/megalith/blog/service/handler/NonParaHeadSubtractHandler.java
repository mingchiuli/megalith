package org.chiu.megalith.blog.service.handler;

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
import static org.chiu.megalith.blog.lang.PushActionEnum.NON_PARA_HEAD_SUBTRACT;

@Component
public class NonParaHeadSubtractHandler extends PushActionAbstractHandler {

    private final StringRedisTemplate redisTemplate;

    public NonParaHeadSubtractHandler(SimpMessagingTemplate simpMessagingTemplate,
                                      StringRedisTemplate redisTemplate,
                                      SimpMessagingTemplate simpMessagingTemplate1) {
        super(simpMessagingTemplate);
        this.redisTemplate = redisTemplate;
        this.simpMessagingTemplate = simpMessagingTemplate1;
    }

    @Override
    public boolean match(PushActionEnum pushActionEnum) {
        return NON_PARA_HEAD_SUBTRACT.equals(pushActionEnum);
    }

    @Override
    @SuppressWarnings("unchecked")
    public void handle(BlogEditPushActionDto dto) {
        String redisKey = dto.getRedisKey();
        FieldEnum fieldEnum = dto.getFieldEnum();
        String userKey = dto.getUserKey();
        Integer indexStart = dto.getIndexStart();
        Integer version = dto.getVersion();

        List<String> resp =  Optional.ofNullable((redisTemplate.execute(LuaScriptUtils.hGetTwoArgs,
                        Collections.singletonList(redisKey),
                        VERSION.getMsg(), fieldEnum.getField())))
                .orElseGet(ArrayList::new);
        String v = resp.getFirst();
        String value = resp.getLast();
        int rawVersion = Integer.parseInt(v);
        int newVersion = dto.getVersion();

        checkVersion(rawVersion, newVersion, userKey);

        value = value.substring(indexStart);

        redisTemplate.execute(LuaScriptUtils.pushActionLua, Collections.singletonList(redisKey),
                fieldEnum.getField(), VERSION.getMsg(),
                value, String.valueOf(version),
                "604800");
    }
}
