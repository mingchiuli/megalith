package org.chiu.megalith.blog.service.handler;

import org.chiu.megalith.blog.dto.BlogEditPushActionDto;
import org.chiu.megalith.blog.lang.PushActionEnum;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

import java.util.*;

import static org.chiu.megalith.blog.lang.MessageActionFieldEnum.VERSION;
import static org.chiu.megalith.blog.lang.PushActionEnum.PARA_TAIL_SUBTRACT;
import static org.chiu.megalith.infra.lang.Const.PARAGRAPH_PREFIX;

@Component
public class ParaTailSubtractHandler extends PushActionAbstractHandler {

    private final StringRedisTemplate redisTemplate;

    public ParaTailSubtractHandler(SimpMessagingTemplate simpMessagingTemplate,
                                   StringRedisTemplate redisTemplate,
                                   SimpMessagingTemplate simpMessagingTemplate1) {
        super(simpMessagingTemplate);
        this.redisTemplate = redisTemplate;
        this.simpMessagingTemplate = simpMessagingTemplate1;
    }

    @Override
    public boolean match(PushActionEnum pushActionEnum) {
        return PARA_TAIL_SUBTRACT.equals(pushActionEnum);
    }

    @Override
    @SuppressWarnings("unchecked")
    public void handle(BlogEditPushActionDto dto) {
        String redisKey = dto.getRedisKey();
        String userKey = dto.getUserKey();
        Integer version = dto.getVersion();
        Integer indexStart = dto.getIndexStart();
        Integer paraNo = dto.getParaNo();

        List<String> resp =  Optional.ofNullable((redisTemplate.execute(LuaScriptUtils.hGetTwoArgs,
                        Collections.singletonList(redisKey),
                        VERSION.getMsg(), PARAGRAPH_PREFIX.getInfo() + paraNo)))
                .orElseGet(ArrayList::new);

        String v = resp.getFirst();
        String value = resp.getLast();
        value = Objects.isNull(value) ? "" : value;
        int rawVersion = Integer.parseInt(v);
        int newVersion = dto.getVersion();

        checkVersion(rawVersion, newVersion, userKey);

        value = value.substring(0, indexStart);

        Map<String, String> subMap = new LinkedHashMap<>();
        subMap.put(PARAGRAPH_PREFIX.getInfo() + paraNo, value);
        subMap.put(VERSION.getMsg(), version.toString());
        redisTemplate.opsForHash().putAll(redisKey, subMap);
    }
}
