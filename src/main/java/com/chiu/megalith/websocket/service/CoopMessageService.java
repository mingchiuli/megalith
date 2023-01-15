package com.chiu.megalith.websocket.service;

import com.chiu.megalith.websocket.dto.impl.ChatInfoDto;
import com.chiu.megalith.websocket.dto.impl.DestroyDto;
import com.chiu.megalith.websocket.dto.impl.QuitDto;
import com.chiu.megalith.websocket.dto.impl.SyncContentDto;


/**
 * @author mingchiuli
 * @create 2022-12-28 3:42 pm
 */
public interface CoopMessageService {
    void chat(ChatInfoDto.Message msg);

    void sync(SyncContentDto.Content msg);

    void destroy(DestroyDto.Bind msg);

    void quit(QuitDto.Bind msg);
}
