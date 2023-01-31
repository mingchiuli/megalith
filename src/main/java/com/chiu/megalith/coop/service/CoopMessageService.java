package com.chiu.megalith.coop.service;

import com.chiu.megalith.coop.dto.impl.ChatDto;
import com.chiu.megalith.coop.dto.impl.DestroyDto;
import com.chiu.megalith.coop.dto.impl.QuitDto;
import com.chiu.megalith.coop.dto.impl.SyncContentDto;


/**
 * @author mingchiuli
 * @create 2022-12-28 3:42 pm
 */
public interface CoopMessageService {
    void chat(ChatDto.Bind msg);

    void syncContent(SyncContentDto.Bind msg);

    void destroy(DestroyDto.Bind msg);

    void quit(QuitDto.Bind msg);
}
