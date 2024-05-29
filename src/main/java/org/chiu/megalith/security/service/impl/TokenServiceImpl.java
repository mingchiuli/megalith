package org.chiu.megalith.security.service.impl;

import org.chiu.megalith.security.token.Claims;
import org.chiu.megalith.security.token.TokenUtils;
import org.chiu.megalith.infra.utils.SecurityUtils;
import org.chiu.megalith.user.service.UserRoleService;
import org.chiu.megalith.user.service.UserService;
import org.chiu.megalith.user.vo.UserEntityVo;
import org.chiu.megalith.security.convertor.UserInfoVoConvertor;
import org.chiu.megalith.security.service.TokenService;
import org.chiu.megalith.security.vo.UserInfoVo;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;


import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.chiu.megalith.infra.lang.Const.ROLE_PREFIX;
import static org.chiu.megalith.infra.lang.Const.TOKEN_PREFIX;

/**
 * @author mingchiuli
 * @create 2023-03-30 4:29 am
 */
@Service
@RequiredArgsConstructor
public class TokenServiceImpl implements TokenService {

    private final TokenUtils<Claims> tokenUtils;

    private final UserService userService;

    private final UserRoleService userRoleService;

    @Value("${blog.jwt.access-token-expire}")
    private long expire;

    @Override
    public Map<String, String> refreshToken() {
        Long userId = SecurityUtils.getLoginUserId();
        List<String> roleCodes = userRoleService.findRoleCodesByUserId(userId);
        roleCodes = roleCodes.stream()
                .map(role -> ROLE_PREFIX.getInfo() + role)
                .toList();
        String accessToken = tokenUtils.generateToken(userId.toString(), roleCodes, expire);
        return Collections.singletonMap("accessToken", TOKEN_PREFIX.getInfo() + accessToken);
    }

    @Override
    public UserInfoVo userinfo() {
        Long userId = SecurityUtils.getLoginUserId();
        UserEntityVo userEntity = userRoleService.findById(userId);

        return UserInfoVoConvertor.convert(userEntity);
    }
}
