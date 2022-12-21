package com.chiu.megalith.backstage.controller;

import com.chiu.megalith.backstage.service.UserService;
import com.chiu.megalith.backstage.vo.UserEntityVo;
import com.chiu.megalith.common.lang.Result;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/sys/user")
@RequiredArgsConstructor
public class UserController {

    private final UserService userService;

    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    @PostMapping("/save")
    public Result<Void> save(@Validated @RequestBody UserEntityVo userEntityVo) {
        userService.saveOrUpdate(userEntityVo);
        return Result.success();
    }







}
