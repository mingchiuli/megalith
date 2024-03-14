package org.chiu.megalith.blog.service.handler;

import org.chiu.megalith.blog.dto.BlogEditPushActionDto;
import org.chiu.megalith.blog.lang.FieldEnum;
import org.chiu.megalith.blog.lang.PushActionEnum;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

import java.util.Collections;

import static org.chiu.megalith.blog.lang.MessageActionFieldEnum.VERSION;
import static org.chiu.megalith.blog.lang.PushActionEnum.NON_PARA_REPLACE;
import static org.chiu.megalith.infra.lang.Const.A_WEEK;

@Component
public class NonParaReplaceHandler extends PushActionAbstractHandler {

    private final StringRedisTemplate redisTemplate;

    public NonParaReplaceHandler(SimpMessagingTemplate simpMessagingTemplate,
                                 StringRedisTemplate redisTemplate,
                                 SimpMessagingTemplate simpMessagingTemplate1) {
        super(simpMessagingTemplate, redisTemplate);
        this.redisTemplate = redisTemplate;
        this.simpMessagingTemplate = simpMessagingTemplate1;
    }

    @Override
    public boolean match(PushActionEnum pushActionEnum) {
        return NON_PARA_REPLACE.equals(pushActionEnum);
    }

    @Override
    protected String getValue(String contentChange, String value, Integer indexStart, Integer indexEnd) {
        return value.substring(0, indexStart) + contentChange + value.substring(indexEnd);
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
                A_WEEK.getInfo());
    }
}
