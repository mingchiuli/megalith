package org.chiu.megalith.coop.service.impl;

import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.utils.JsonUtils;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.chiu.megalith.blog.vo.BlogEntityVo;
import org.chiu.megalith.coop.config.CoopRabbitConfig;
import org.chiu.megalith.coop.dto.BaseDto;
import org.chiu.megalith.coop.dto.impl.FinishCoopDto;
import org.chiu.megalith.coop.dto.impl.QuitCoopDto;
import org.chiu.megalith.coop.dto.impl.SyncContentDto;
import org.chiu.megalith.coop.lang.OperateType;
import org.chiu.megalith.coop.service.CoopMessageService;
import org.chiu.megalith.coop.vo.UserEntityVo;
import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.service.UserService;

import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.util.Collections;

/**
 * @author mingchiuli
 * @create 2022-12-28 3:43 pm
 */
@Service
public class CoopMessageServiceImpl extends BaseMessageService implements CoopMessageService {

    public CoopMessageServiceImpl(StringRedisTemplate redisTemplate, 
                                  JsonUtils jsonUtils,
                                  RabbitTemplate rabbitTemplate,
                                  UserService userService) {
        super(redisTemplate, jsonUtils, rabbitTemplate);
        this.userService = userService;
    }

    private final UserService userService;

    @Override
    public void syncContent(SyncContentDto msg) {
        String changeContent = msg.getContent();
        String type = msg.getOperateType();
        Integer offset = msg.getOffset();
        Object blogId = msg.getBlogId();
    
        HashOperations<String, String, String> operations = redisTemplate.opsForHash();
        String contentValue = operations.get(Const.COOP_PREFIX.getInfo() + blogId, Const.BLOG_CONTENT.getInfo());
        BlogEntityVo blogVo = jsonUtils.readValue(contentValue, BlogEntityVo.class);
        String content = blogVo.getContent();

        OperateType operateType = OperateType.valueOf(type);
        String prefix = content.substring(0, offset);
        String suffix = content.substring(offset, content.length());

        if (operateType.equals(OperateType.ADD)) {
            content = prefix + changeContent + suffix;
        } else {
            suffix = suffix.substring(offset);
            content = prefix + suffix;
        }
        blogVo.setContent(content);
        
        operations.put(Const.COOP_PREFIX.getInfo() + blogId, Const.BLOG_CONTENT.getInfo(), jsonUtils.writeValueAsString(blogVo));
        sendToOtherUsers(msg);
    }

    @Override
    public void destroySession(FinishCoopDto msg) {
        sendToOtherUsers(msg);
    }

    @Override
    public void quitEdit(QuitCoopDto msg) {
        sendToOtherUsers(msg);
        redisTemplate.opsForHash().delete(Const.COOP_PREFIX.getInfo() + msg.getBlogId(), msg.getFromId().toString());
    }

    @Override
    public void setUserToRedisSession(Long userId, Long blogId) {
        UserEntity userEntity = userService.findById(userId);
        var userEntityVo = UserEntityVo.builder()
                .id(userId)
                .avatar(userEntity.getAvatar())
                .nickname(userEntity.getNickname())
                .nodeMark(CoopRabbitConfig.nodeMark)
                .build();

        redisTemplate.execute(LuaScriptUtils.sendUserToSessionLua,
                Collections.singletonList(Const.COOP_PREFIX.getInfo() + blogId),
                userId.toString(), jsonUtils.writeValueAsString(userEntityVo), "21600");
    }

    private void sendToOtherUsers(BaseDto msg) {
        Long blogId = msg.getBlogId();
        Long userId = msg.getFromId();
        super.sendToOtherUsers(blogId, userId);
    }

    @Override
    public BlogEntityVo getBlogContent(Long blogId) {
        HashOperations<String, String, String> operations = redisTemplate.opsForHash();
        String contentValue = operations.get(Const.COOP_PREFIX.getInfo() + blogId, Const.BLOG_CONTENT.getInfo());
        return jsonUtils.readValue(contentValue, BlogEntityVo.class);   
    }

}
