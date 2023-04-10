package com.chiu.megalith.coop.service;

import com.chiu.megalith.coop.dto.*;


/**
 * @author mingchiuli
 * @create 2022-12-28 3:42 pm
 */
public interface CoopMessageService {
    void chatUser(MessageDto.BaseBind msg);

    void syncBlog(MessageDto.BaseBind msg);

    void submitBlog(MessageDto.BaseBind msg);

    void quitBlog(MessageDto.BaseBind msg);

    void setUserToRedisSession(Long userId, Long blogId);

}
