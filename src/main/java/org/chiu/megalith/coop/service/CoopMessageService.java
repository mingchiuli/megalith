package org.chiu.megalith.coop.service;

import org.chiu.megalith.coop.dto.*;


/**
 * @author mingchiuli
 * @create 2022-12-28 3:42 pm
 */
public interface CoopMessageService {
    
    void chatUser(BaseDto msg);

    void syncBlog(BaseDto msg);

    void submitBlog(BaseDto msg);

    void quitBlog(BaseDto msg);

    void setUserToRedisSession(Long userId, Long blogId);

}
