package org.chiu.megalith.security.controller;

import org.chiu.megalith.security.service.CodeService;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.manage.service.UserService;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author mingchiuli
 * @create 2022-11-27 6:32 pm
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/code")
public class CodeController {

    private final CodeService codeService;

    private final UserService userService;

    @GetMapping("/email")
    public Result<Void> createEmailCode(@RequestParam(value = "loginEmail") String loginEmail) {

        userService.findByEmail(loginEmail);
        codeService.createEmailCode(loginEmail);
        return Result.success();
    }

    @GetMapping("/sms")
    public Result<Void> createSmsCode(@RequestParam(value = "loginSMS") String loginSMS) {
        codeService.createSMSCode(loginSMS);
        return Result.success();
    }
}
