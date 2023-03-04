package com.chiu.megalith.security.controller;

import com.chiu.megalith.security.service.EmailCodeService;
import com.chiu.megalith.common.lang.Result;
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
public class EmailCodeController {
    private final EmailCodeService emailCodeService;

    @GetMapping("/email")
    public Result<Void> createEmailCode(@RequestParam(value = "loginEmail") String loginEmail) {
        emailCodeService.createEmailCode(loginEmail);
        return Result.success();
    }
}
