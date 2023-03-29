package com.chiu.megalith.security.controller;

import com.chiu.megalith.base.lang.Result;
import com.chiu.megalith.security.service.TokenService;
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
    @PreAuthorize("hasRole('REFRESH')")
    public Result<Map<String, String>> refreshToken() {
        Map<String, String> resp = tokenService.refreshToken();
        return Result.success(resp);
    }
}
