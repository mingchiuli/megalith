package org.chiu.megalith.blog.service;

import org.chiu.megalith.blog.req.BlogEditPushActionReq;

public interface BlogMessageService {

    void pushAction(BlogEditPushActionReq req, Long userId);

}
