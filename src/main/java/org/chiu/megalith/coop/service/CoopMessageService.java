package org.chiu.megalith.coop.service;

import org.chiu.megalith.coop.vo.BlogEntityVo;
import org.chiu.megalith.coop.req.FinishCoopReq;
import org.chiu.megalith.coop.req.QuitCoopReq;
import org.chiu.megalith.coop.req.SyncContentReq;


/**
 * @author mingchiuli
 * @create 2022-12-28 3:42 pm
 */
public interface CoopMessageService {
    
    void syncContent(SyncContentReq msg);

    void destroySession(FinishCoopReq msg);

    void quitEdit(QuitCoopReq msg);

    void setUserToRedisSession(Long userId, Long blogId);

    BlogEntityVo getBlogContent(Long blogId);

}
