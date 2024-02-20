package org.chiu.megalith.blog.service.handler;

import org.chiu.megalith.blog.dto.BlogEditPushActionDto;
import org.chiu.megalith.blog.lang.PushActionEnum;

public abstract class PushActionAbstractHandler {

    public abstract boolean match(PushActionEnum pushActionEnum);

    public abstract void handle(BlogEditPushActionDto dto);
}
