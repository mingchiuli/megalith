package com.chiu.megalith.security.service;

import com.chiu.megalith.security.vo.UserInfoVo;

import java.util.Map;

/**
 * @author mingchiuli
 * @create 2023-03-30 4:29 am
 */
public interface TokenService {

    Map<String, String> refreshToken();

    UserInfoVo userinfo();
}
