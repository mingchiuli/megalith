package com.chiu.megalith.websocket.service;

import java.util.Map;

/**
 * @author mingchiuli
 * @create 2022-12-26 1:05 am
 */
public interface InitCoopService {
    Map<String, Object> initCoop(Long blogId, Integer orderNumber);

}
