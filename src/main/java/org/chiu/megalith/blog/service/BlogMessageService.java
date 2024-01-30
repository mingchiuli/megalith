package org.chiu.megalith.blog.service;

import org.chiu.megalith.blog.req.BlogEditPushActionReq;
import org.chiu.megalith.blog.req.BlogEditPushAllReq;

public interface BlogMessageService {

    void pushAction(BlogEditPushActionReq req, Long userId);

}
