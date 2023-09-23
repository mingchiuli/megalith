package org.chiu.megalith.coop.service;

import org.chiu.megalith.coop.vo.BlogEntityVo;
import org.chiu.megalith.coop.vo.FinishCoopVo;
import org.chiu.megalith.coop.vo.QuitCoopVo;
import org.chiu.megalith.coop.vo.SyncContentVo;


/**
 * @author mingchiuli
 * @create 2022-12-28 3:42 pm
 */
public interface CoopMessageService {
    
    void syncContent(SyncContentVo msg);

    void destroySession(FinishCoopVo msg);

    void quitEdit(QuitCoopVo msg);

    void setUserToRedisSession(Long userId, Long blogId);

    BlogEntityVo getBlogContent(Long blogId);

}
