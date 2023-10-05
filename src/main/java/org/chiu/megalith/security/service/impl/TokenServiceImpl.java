package org.chiu.megalith.security.service.impl;

import org.chiu.megalith.infra.jwt.JwtUtils;
import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.service.UserService;
import org.chiu.megalith.security.service.TokenService;
import org.chiu.megalith.security.vo.UserInfoVo;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Map;

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
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        Long userId = Long.valueOf(authentication.getName());
        UserEntity user = userService.findById(userId);
        String accessToken = jwtUtils.generateToken(authentication.getName(), user.getRole(), expire);
        return Collections.singletonMap("accessToken", "Bearer " + accessToken);
    }

    @Override
    public UserInfoVo userinfo() {
        Long userId = Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());
        UserEntity userEntity = userService.findById(userId);

        return UserInfoVo.builder()
                .avatar(userEntity.getAvatar())
                .nickname(userEntity.getNickname())
                .build();
    }
}
