package com.chiu.megalith.security.service.impl;

import com.chiu.megalith.infra.jwt.JwtUtils;
import com.chiu.megalith.security.service.TokenService;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
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

    @Value("${blog.jwt.access-token-expire}")
    private long expire;

    @Override
    public Map<String, String> refreshToken() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String userId = authentication.getName();

        String accessToken = jwtUtils.generateToken(
                userId,
                authentication.getAuthorities().stream()
                        .findFirst()
                        .map(GrantedAuthority::getAuthority)
                        .orElseThrow(),
                expire);
        return Collections.singletonMap("accessToken", accessToken);
    }
}
