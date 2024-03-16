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
import static org.chiu.megalith.blog.lang.PushActionEnum.STATUS;

@Component
public class StatusHandler extends PushActionAbstractHandler {

    private final StringRedisTemplate redisTemplate;

    public StatusHandler(SimpMessagingTemplate simpMessagingTemplate,
                         StringRedisTemplate redisTemplate,
                         SimpMessagingTemplate simpMessagingTemplate1) {
        super(simpMessagingTemplate, redisTemplate);
        this.redisTemplate = redisTemplate;
        this.simpMessagingTemplate = simpMessagingTemplate1;
    }

    @Override
    public boolean match(PushActionEnum pushActionEnum) {
        return STATUS.equals(pushActionEnum);
    }

    @Override
    protected String getValue(String contentChange, String value, Integer indexStart, Integer indexEnd) {
        return contentChange;
    }

    @Override
    protected String getRedisValue(BlogEditPushActionDto dto) {
        return dto.getFieldEnum().getField();
    }

    @Override
    protected void setContent(BlogEditPushActionDto dto, String value, Integer version) {
        FieldEnum fieldEnum = dto.getFieldEnum();
        String redisKey = dto.getRedisKey();

        redisTemplate.execute(LuaScriptUtils.pushActionLua, Collections.singletonList(redisKey),
                fieldEnum.getField(), VERSION.getMsg(),
                value, String.valueOf(version),
                "604800");
    }
}
