package org.chiu.megalith.coop.service;

import org.chiu.megalith.blog.vo.BlogEntityVo;
import org.chiu.megalith.coop.dto.impl.FinishCoopDto;
import org.chiu.megalith.coop.dto.impl.QuitCoopDto;
import org.chiu.megalith.coop.dto.impl.SyncContentDto;


/**
 * @author mingchiuli
 * @create 2022-12-28 3:42 pm
 */
public interface CoopMessageService {
    
    void syncContent(SyncContentDto msg);

    void destroySession(FinishCoopDto msg);

    void quitEdit(QuitCoopDto msg);

    void setUserToRedisSession(Long userId, Long blogId);

    BlogEntityVo getBlogContent(Long blogId);

}
