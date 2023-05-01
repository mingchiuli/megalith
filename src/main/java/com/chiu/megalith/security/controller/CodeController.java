package com.chiu.megalith.security.controller;

import com.chiu.megalith.security.service.CodeService;
import com.chiu.megalith.infra.lang.Result;
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
        Boolean success = codeService.createEmailCode(loginEmail);
        if (success) {
            return Result.success();
        }
        return Result.fail("code exist");
    }
}
