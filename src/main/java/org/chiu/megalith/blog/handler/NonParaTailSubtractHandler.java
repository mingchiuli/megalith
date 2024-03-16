package org.chiu.megalith.blog.handler;

import org.chiu.megalith.blog.dto.BlogEditPushActionDto;
import org.chiu.megalith.blog.lang.FieldEnum;
import org.chiu.megalith.blog.lang.PushActionEnum;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

import java.util.Collections;

import static org.chiu.megalith.blog.lang.MessageActionFieldEnum.VERSION;
import static org.chiu.megalith.blog.lang.PushActionEnum.NON_PARA_TAIL_SUBTRACT;

@Component
public class NonParaTailSubtractHandler extends PushActionAbstractHandler {

    private final StringRedisTemplate redisTemplate;

    public NonParaTailSubtractHandler(SimpMessagingTemplate simpMessagingTemplate,
                                      StringRedisTemplate redisTemplate) {
        super(simpMessagingTemplate, redisTemplate);
        this.redisTemplate = redisTemplate;
    }

    @Override
    public boolean match(PushActionEnum pushActionEnum) {
        return NON_PARA_TAIL_SUBTRACT.equals(pushActionEnum);
    }

    @Override
    protected String getValue(String contentChange, String value, Integer indexStart, Integer indexEnd) {
        return value.substring(0, indexStart);
    }

    @Override
    protected String getRedisValue(BlogEditPushActionDto dto) {
        return dto.getFieldEnum().getField();
    }

    @Override
    protected void setContent(BlogEditPushActionDto dto, String value, Integer version) {
        String redisKey = dto.getRedisKey();
        FieldEnum fieldEnum = dto.getFieldEnum();
        redisTemplate.execute(LuaScriptUtils.pushActionLua, Collections.singletonList(redisKey),
                fieldEnum.getField(), VERSION.getMsg(),
                value, String.valueOf(version),
                "604800");
    }
}
