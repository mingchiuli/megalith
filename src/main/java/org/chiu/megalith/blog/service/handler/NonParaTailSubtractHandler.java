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
import static org.chiu.megalith.blog.lang.PushActionEnum.NON_PARA_TAIL_SUBTRACT;

@Component
public class NonParaTailSubtractHandler extends PushActionAbstractHandler {

    private final StringRedisTemplate redisTemplate;

    public NonParaTailSubtractHandler(SimpMessagingTemplate simpMessagingTemplate,
                                      StringRedisTemplate redisTemplate) {
        super(simpMessagingTemplate);
        this.redisTemplate = redisTemplate;
    }

    @Override
    public boolean match(PushActionEnum pushActionEnum) {
        return NON_PARA_TAIL_SUBTRACT.equals(pushActionEnum);
    }

    @Override
    @SuppressWarnings("unchecked")
    public void handle(BlogEditPushActionDto dto) {
        String redisKey = dto.getRedisKey();
        FieldEnum fieldEnum = dto.getFieldEnum();
        String userKey = dto.getUserKey();
        Integer version = dto.getVersion();
        Integer indexStart = dto.getIndexStart();

        List<String> resp =  Optional.ofNullable((redisTemplate.execute(LuaScriptUtils.hGetTwoArgs,
                        Collections.singletonList(redisKey),
                        VERSION.getMsg(), fieldEnum.getField())))
                .orElseGet(ArrayList::new);
        String v = resp.getFirst();
        String value = resp.getLast();
        int rawVersion = Integer.parseInt(v);
        int newVersion = dto.getVersion();

        checkVersion(rawVersion, newVersion, userKey);

        value = value.substring(0, indexStart);

        redisTemplate.execute(LuaScriptUtils.pushActionLua, Collections.singletonList(redisKey),
                fieldEnum.getField(), VERSION.getMsg(),
                value, String.valueOf(version),
                "604800");
    }
}
