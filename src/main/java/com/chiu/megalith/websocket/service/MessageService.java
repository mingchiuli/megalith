package com.chiu.megalith.websocket.service;

import com.chiu.megalith.websocket.dto.impl.ChatInfoDto;

import java.security.Principal;

/**
 * @author mingchiuli
 * @create 2022-12-28 3:42 pm
 */
public interface MessageService {
    void chat(Principal user, ChatInfoDto.Message msg);
}
