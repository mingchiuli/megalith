package org.chiu.megalith.blog.service.impl;


import jakarta.annotation.PostConstruct;
import lombok.SneakyThrows;
import org.chiu.megalith.blog.convertor.BlogEditPushActionDtoConvertor;
import org.chiu.megalith.blog.dto.BlogEditPushActionDto;
import org.chiu.megalith.blog.lang.PushActionEnum;
import org.chiu.megalith.blog.req.BlogEditPushActionReq;
import org.chiu.megalith.blog.service.BlogMessageService;
import org.chiu.megalith.blog.service.handler.PushActionAbstractHandler;
import org.chiu.megalith.infra.key.KeyFactory;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class BlogMessageServiceImpl implements BlogMessageService {

    private final SimpMessagingTemplate simpMessagingTemplate;

    private final List<PushActionAbstractHandler> handlers;

    private final Map<PushActionEnum, PushActionAbstractHandler> handlerMap = new HashMap<>();

    @PostConstruct
    private void init() {
        for (PushActionAbstractHandler handler : handlers) {
            for (PushActionEnum value : PushActionEnum.values()) {
                if (handler.match(value)) {
                    handlerMap.put(value, handler);
                    break;
                }
            }
        }
    }

    @SneakyThrows
    @Override
    public void pushAction(BlogEditPushActionReq req, Long userId) {
        Long blogId = req.getId();
        Integer operateTypeCode = req.getOperateTypeCode();
        String redisKey = KeyFactory.createBlogEditRedisKey(userId, blogId);
        String userKey = KeyFactory.createPushContentIdentityKey(userId, blogId);
        PushActionEnum pushActionEnum = PushActionEnum.getInstance(operateTypeCode);
        BlogEditPushActionDto dto = BlogEditPushActionDtoConvertor.convert(redisKey, userKey, req);

        PushActionAbstractHandler handler = handlerMap.get(pushActionEnum);
        try {
            handler.handle(dto);
        } catch (IndexOutOfBoundsException e) {
            simpMessagingTemplate.convertAndSend("/edits/push/all/" + userKey, "ALL");
            throw e;
        }
    }

}
