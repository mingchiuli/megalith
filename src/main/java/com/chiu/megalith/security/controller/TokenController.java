package com.chiu.megalith.security.controller;

import com.chiu.megalith.infra.lang.Result;
import com.chiu.megalith.security.service.TokenService;
import com.chiu.megalith.security.vo.UserInfoVo;
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
        Map<String, String> resp = tokenService.refreshToken();
        return Result.success(resp);
    }

    @GetMapping("/userinfo")
    public Result<UserInfoVo> userinfo() {
        UserInfoVo user = tokenService.userinfo();
        return Result.success(user);
    }
}
