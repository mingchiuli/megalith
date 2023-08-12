package org.chiu.megalith.coop.service;

import org.chiu.megalith.coop.dto.*;


/**
 * @author mingchiuli
 * @create 2022-12-28 3:42 pm
 */
public interface CoopMessageService {
    
    void syncContent(BaseDto msg);

    void submitBlog(BaseDto msg);

    void quitEdit(BaseDto msg);

    void setUserToRedisSession(Long userId, Long blogId);

}
