package com.chiu.megalith.coop.service;

import com.chiu.megalith.coop.dto.*;


/**
 * @author mingchiuli
 * @create 2022-12-28 3:42 pm
 */
public interface CoopMessageService {
    void chat(MessageDto.BaseBind msg);

    void syncContent(MessageDto.BaseBind msg);

    void destroy(MessageDto.BaseBind msg);

    void quit(MessageDto.BaseBind msg);

    void setUserToRedisSession(Long userId, Long blogId);

}
