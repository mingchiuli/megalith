package org.chiu.megalith.blog.service.impl;

import jakarta.annotation.PostConstruct;
import lombok.SneakyThrows;
import org.chiu.megalith.blog.req.BlogEditPushActionReq;
import org.chiu.megalith.blog.service.BlogMessageService;
import org.chiu.megalith.infra.key.KeyFactory;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import org.springframework.util.ResourceUtils;
import java.nio.charset.StandardCharsets;
import java.util.*;



@Service
@RequiredArgsConstructor
public class BlogMessageServiceImpl implements BlogMessageService {

    private final SimpMessagingTemplate simpMessagingTemplate;

    private final StringRedisTemplate redisTemplate;

    private final ResourceLoader resourceLoader;

    private String script;


    @PostConstruct
    @SneakyThrows
    private void init() {
        Resource resource = resourceLoader.getResource(ResourceUtils.CLASSPATH_URL_PREFIX + "script/push-action.lua");
        script = resource.getContentAsString(StandardCharsets.UTF_8);
    }

    @Override
    public void pushAction(BlogEditPushActionReq req, Long userId) {
        Long blogId = req.getId();
        String contentChange = req.getContentChange();
        Integer operateTypeCode = req.getOperateTypeCode();
        Integer version = req.getVersion();
        Integer indexStart = req.getIndexStart();
        Integer indexEnd = req.getIndexEnd();
        String field = req.getField();
        Integer paraNo = req.getParaNo();
        Boolean reconnected = req.getReconnected();

        String userKey = KeyFactory.createPushContentIdentityKey(userId, blogId);

        if (Boolean.TRUE.equals(reconnected)) {
            simpMessagingTemplate.convertAndSend("/edits/push/all/" + userKey, "ALL");
            return;
        }

        String redisKey = KeyFactory.createBlogEditRedisKey(userId, blogId);

        Long execute = redisTemplate.execute(RedisScript.of(script, Long.class), Collections.singletonList(redisKey),
                contentChange,
                operateTypeCode.toString(),
                version.toString(),
                Objects.nonNull(indexStart) ? indexStart.toString() : null,
                Objects.nonNull(indexEnd) ? indexEnd.toString() : null,
                Objects.nonNull(field) ? field : null,
                Objects.nonNull(paraNo) ? paraNo.toString() : null);

        if (Long.valueOf(-1).equals(execute)) {
            simpMessagingTemplate.convertAndSend("/edits/push/all/" + userKey, "ALL");
        }
    }

}
