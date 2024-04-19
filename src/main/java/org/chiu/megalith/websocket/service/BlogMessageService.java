package org.chiu.megalith.websocket.service;

import org.chiu.megalith.websocket.req.BlogEditPushActionReq;

public interface BlogMessageService {

    void pushAction(BlogEditPushActionReq req, Long userId);

}
