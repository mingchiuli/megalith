package com.chiu.megalith.websocket.service;

import com.chiu.megalith.websocket.vo.InitCoopVo;

import java.util.Map;

/**
 * @author mingchiuli
 * @create 2022-12-26 1:05 am
 */
public interface InitCoopService {
    InitCoopVo initCoop(Long blogId, Integer orderNumber);

}
