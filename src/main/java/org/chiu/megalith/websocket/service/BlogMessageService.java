package org.chiu.megalith.websocket.service;

import org.chiu.megalith.websocket.req.BlogEditPushActionReq;
import org.chiu.megalith.websocket.req.BlogEditPushAllReq;
import org.chiu.megalith.websocket.vo.BlogEditVo;

public interface BlogMessageService {

    void pushAction(BlogEditPushActionReq req, Long userId);

    void pushAll(BlogEditPushAllReq blog, Long userId);

    BlogEditVo findEdit(Long id, Long userId);
}
