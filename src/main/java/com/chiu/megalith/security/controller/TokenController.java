package com.chiu.megalith.security.controller;

import com.chiu.megalith.base.jwt.JwtUtils;
import com.chiu.megalith.base.lang.Result;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collections;
import java.util.Map;

/**
 * @author mingchiuli
 * @create 2023-03-29 12:58 am
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/token")
public class TokenController {

    private final JwtUtils jwtUtils;

    @Value("${blog.jwt.access-token-expire}")
    private long expire;

    @GetMapping("/refresh")
    @PreAuthorize("hasRole('REFRESH')")
    public Result<Map<String, String>> createEmailCode() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String userId = authentication.getName();

        String accessToken = jwtUtils.generateToken(
                userId,
                authentication
                        .getAuthorities()
                        .stream()
                        .findFirst()
                        .map(GrantedAuthority::getAuthority)
                        .orElseThrow(),
                expire
        );
        Map<String, String> resp = Collections.singletonMap("accessToken", accessToken);
        return Result.success(resp);
    }
}
