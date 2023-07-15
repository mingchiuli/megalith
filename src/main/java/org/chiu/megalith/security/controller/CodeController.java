package org.chiu.megalith.security.controller;

import org.chiu.megalith.security.service.CodeService;
import org.chiu.megalith.infra.lang.Result;
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

    @GetMapping("/email")
    public Result<Void> createEmailCode(@RequestParam(value = "loginEmail") String loginEmail) {
        boolean success = codeService.createEmailCode(loginEmail);
        return success ?
                Result.success() :
                Result.fail("code exist");
    }

    @GetMapping("/sms")
    public Result<Void> createSmsCode(@RequestParam(value = "loginSMS") String loginSMS) {
        boolean success = codeService.createSMSCode(loginSMS);
        return success ?
                Result.success() :
                Result.fail("code exist");
    }
}
