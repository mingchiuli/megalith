package com.chiu.megalith.backstage.controller;

import com.chiu.megalith.backstage.entity.UserEntity;
import com.chiu.megalith.backstage.service.UserService;
import com.chiu.megalith.backstage.vo.UserEntityVo;
import com.chiu.megalith.common.lang.Result;
import com.chiu.megalith.common.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

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

    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    @GetMapping("/status/{id}/{status}")
    public Result<Void> changeUserStatus(@PathVariable(value = "id") Long userId, @PathVariable(value = "status") Integer status) {
        userService.changeUserStatus(userId, status);
        return Result.success();
    }


    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    @GetMapping("/page/{currentPage}")
    public Result<PageAdapter<UserEntity>> page(@PathVariable(value = "currentPage") Integer currentPage, @RequestParam(value = "size", defaultValue = "5") Integer size) {
        PageAdapter<UserEntity> page = userService.listPage(currentPage, size);
        return Result.success(page);
    }

    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    @PostMapping("/delete")
    public Result<Void> page(@RequestBody List<Long> ids) {
        userService.deleteUsers(ids);
        return Result.success();
    }

    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    @GetMapping("/info/{id}")
    public Result<UserEntity> info(@PathVariable(value = "id") Long id) {
        UserEntity user = userService.findByIdWithoutPassword(id);
        return Result.success(user);
    }

}
