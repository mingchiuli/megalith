package org.chiu.megalith.security.service.impl;

import org.chiu.megalith.infra.jwt.JwtUtils;
import org.chiu.megalith.infra.utils.SecurityUtils;
import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.service.UserService;
import org.chiu.megalith.security.service.TokenService;
import org.chiu.megalith.security.vo.UserInfoVo;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.Collections;
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

    private final JwtUtils jwtUtils;

    private final UserService userService;

    @Value("${blog.jwt.access-token-expire}")
    private long expire;

    @Override
    public Map<String, String> refreshToken() {
        Long userId = SecurityUtils.getLoginUserId();
        UserEntity user = userService.findById(userId);
        String accessToken = jwtUtils.generateToken(userId.toString(), ROLE_PREFIX.getInfo() + user.getRole(), expire);
        return Collections.singletonMap("accessToken", TOKEN_PREFIX.getInfo() + accessToken);
    }

    @Override
    public UserInfoVo userinfo() {
        Long userId = SecurityUtils.getLoginUserId();
        UserEntity userEntity = userService.findById(userId);

        return UserInfoVo.builder()
                .avatar(userEntity.getAvatar())
                .nickname(userEntity.getNickname())
                .build();
    }
}
