package org.chiu.megalith.blog.service.handler;

import org.chiu.megalith.blog.dto.BlogEditPushActionDto;
import org.chiu.megalith.blog.lang.PushActionEnum;
import org.springframework.messaging.simp.SimpMessagingTemplate;

public abstract class PushActionAbstractHandler {

    protected SimpMessagingTemplate simpMessagingTemplate;

    public PushActionAbstractHandler(SimpMessagingTemplate simpMessagingTemplate) {
        this.simpMessagingTemplate = simpMessagingTemplate;
    }

    public abstract boolean match(PushActionEnum pushActionEnum);

    public abstract void handle(BlogEditPushActionDto dto);

    protected void checkVersion(Integer oldVersion, Integer newVersion, String userKey) {
        if (newVersion != oldVersion + 1) {
            // 前端向服务端推全量
            simpMessagingTemplate.convertAndSend("/edits/push/all/" + userKey, "ALL");
            throw new IllegalArgumentException();
        }
    }
}
