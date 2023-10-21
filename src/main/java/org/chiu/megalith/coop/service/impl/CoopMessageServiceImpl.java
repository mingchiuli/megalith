package org.chiu.megalith.coop.service.impl;

import org.chiu.megalith.coop.dto.impl.FinishCoopDto;
import org.chiu.megalith.coop.dto.impl.QuitCoopDto;
import org.chiu.megalith.coop.dto.impl.SyncContentDto;
import org.chiu.megalith.coop.req.FinishCoopReq;
import org.chiu.megalith.coop.req.QuitCoopReq;
import org.chiu.megalith.coop.req.SyncContentReq;
import org.chiu.megalith.coop.vo.*;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.utils.JsonUtils;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
import org.chiu.megalith.manage.req.BlogEntityReq;
import org.chiu.megalith.coop.config.CoopRabbitConfig;
import org.chiu.megalith.coop.lang.OperateType;
import org.chiu.megalith.coop.service.CoopMessageService;
import org.chiu.megalith.coop.dto.UserEntityDto;
import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.service.UserService;

import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * @author mingchiuli
 * @create 2022-12-28 3:43 pm
 */
@Service
public class CoopMessageServiceImpl extends BaseCoopService implements CoopMessageService {

    private StringRedisTemplate redisTemplate;

    private RabbitTemplate rabbitTemplate;

    private JsonUtils jsonUtils;

    private final UserService userService;

    public CoopMessageServiceImpl(StringRedisTemplate redisTemplate, JsonUtils jsonUtils, RabbitTemplate rabbitTemplate, UserService userService) {
        super(redisTemplate, jsonUtils, rabbitTemplate);
        this.userService = userService;
    }

    @Override
    public void syncContent(SyncContentReq msg) {
        String changeContent = msg.getContent();
        String type = msg.getOperateType();
        Integer offset = msg.getOffset();
        Long blogId = msg.getBlogId();
    
        HashOperations<String, String, String> operations = redisTemplate.opsForHash();
        String contentValue = operations.get(Const.COOP_PREFIX.getInfo() + blogId, Const.BLOG_CONTENT.getInfo());
        BlogEntityReq blogVo = jsonUtils.readValue(contentValue, BlogEntityReq.class);
        String content = blogVo.getContent();

        OperateType operateType = OperateType.valueOf(type);
        String prefix = content.substring(0, offset);
        String suffix = content.substring(offset);

        if (operateType.equals(OperateType.ADD)) {
            content = prefix + changeContent + suffix;
        } else if (operateType.equals(OperateType.DEL)){
            suffix = suffix.substring(offset);
            content = prefix + suffix;
        }
        blogVo.setContent(content);
        
        operations.put(Const.COOP_PREFIX.getInfo() + blogId, Const.BLOG_CONTENT.getInfo(), jsonUtils.writeValueAsString(blogVo));

        SyncContentDto syncContentDto = new SyncContentDto();
        syncContentDto.setContent(changeContent);
        syncContentDto.setOffset(offset);
        syncContentDto.setBlogId(blogId);
        syncContentDto.setOperateType(type);
        sendToOtherUsers(syncContentDto);
    }

    @Override
    public void destroySession(FinishCoopReq msg) {
        FinishCoopDto finishCoopDto = new FinishCoopDto();
        finishCoopDto.setBlogId(msg.getBlogId());
        finishCoopDto.setFromId(msg.getFromId());
        sendToOtherUsers(finishCoopDto);
    }

    @Override
    public void quitEdit(QuitCoopReq msg) {
        QuitCoopDto quitCoopDto = new QuitCoopDto();
        quitCoopDto.setBlogId(msg.getBlogId());
        quitCoopDto.setFromId(msg.getFromId());
        sendToOtherUsers(quitCoopDto);
        redisTemplate.opsForHash().delete(Const.COOP_PREFIX.getInfo() + msg.getBlogId(), msg.getFromId().toString());
    }

    @Override
    public void setUserToRedisSession(Long userId, Long blogId) {
        UserEntity userEntity = userService.findById(userId);
        var userEntityDto = UserEntityDto.builder()
                .id(userId)
                .avatar(userEntity.getAvatar())
                .nickname(userEntity.getNickname())
                .nodeMark(CoopRabbitConfig.nodeMark)
                .build();

        redisTemplate.execute(LuaScriptUtils.sendUserToSessionLua,
                Collections.singletonList(Const.COOP_PREFIX.getInfo() + blogId),
                userId.toString(), jsonUtils.writeValueAsString(userEntityDto), "21600");
    }

    @Override
    public BlogEntityVo getBlogContent(Long blogId) {
        HashOperations<String, String, String> operations = redisTemplate.opsForHash();
        String contentValue = operations.get(Const.COOP_PREFIX.getInfo() + blogId, Const.BLOG_CONTENT.getInfo());
        return jsonUtils.readValue(contentValue, BlogEntityVo.class);
    }

}
