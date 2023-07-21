package org.chiu.megalith.security.controller;

import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.security.service.TokenService;
import org.chiu.megalith.security.vo.UserInfoVo;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

/**
 * @author mingchiuli
 * @create 2023-03-29 12:58 am
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/token")
public class TokenController {

    private final TokenService tokenService;

    @GetMapping("/refresh")
    @PreAuthorize("hasRole('REFRESH_TOKEN')")
    public Result<Map<String, String>> refreshToken() {
        return Result.success(tokenService::refreshToken);
    }

    @GetMapping("/userinfo")
    public Result<UserInfoVo> userinfo() {
        return Result.success(tokenService::userinfo);
    }
}
