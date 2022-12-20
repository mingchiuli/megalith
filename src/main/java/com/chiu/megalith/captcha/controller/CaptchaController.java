package com.chiu.megalith.captcha.controller;

import com.chiu.megalith.captcha.dto.CaptchaDto;
import com.chiu.megalith.captcha.service.CaptchaService;
import com.chiu.megalith.common.lang.Result;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * @author mingchiuli
 * @create 2022-11-27 6:32 pm
 */
@RestController
@RequiredArgsConstructor
public class CaptchaController {
    private final CaptchaService captchaService;

    @GetMapping("/captcha")
    public Result<CaptchaDto> createCaptcha() {
        CaptchaDto captcha = captchaService.createCaptcha();
        return Result.success(captcha);
    }
}
