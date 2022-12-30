package com.chiu.megalith.websocket.service;

import com.chiu.megalith.websocket.dto.impl.ChatInfoDto;
import com.chiu.megalith.websocket.dto.impl.DestroyDto;
import com.chiu.megalith.websocket.dto.impl.QuitDto;
import com.chiu.megalith.websocket.dto.impl.SyncContentDto;

import java.security.Principal;

/**
 * @author mingchiuli
 * @create 2022-12-28 3:42 pm
 */
public interface MessageService {
    void chat(Principal user, ChatInfoDto.Message msg);

    void sync(Principal user, SyncContentDto.Content msg);

    void destroy(Principal user, DestroyDto.Bind msg);

    void quit(Principal user, QuitDto.Bind msg);
}
