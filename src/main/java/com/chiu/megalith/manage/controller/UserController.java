package com.chiu.megalith.manage.controller;

import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.service.UserService;
import com.chiu.megalith.manage.vo.UserEntityVo;
import com.chiu.megalith.common.lang.Result;
import com.chiu.megalith.common.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping(value = "/sys/user")
@RequiredArgsConstructor
public class UserController {

    private final UserService userService;

    @PostMapping("/save")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> save(@Validated @RequestBody UserEntityVo userEntityVo) {
        userService.saveOrUpdate(userEntityVo);
        return Result.success();
    }

    @GetMapping("/status/{id}/{status}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> changeUserStatus(@PathVariable(value = "id") String userId,
                                         @PathVariable(value = "status") Integer status) {
        userService.changeUserStatus(userId, status);
        return Result.success();
    }

    @GetMapping("/page/{currentPage}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<PageAdapter<UserEntity>> page(@PathVariable(value = "currentPage") Integer currentPage,
                                                @RequestParam(value = "size", defaultValue = "5") Integer size) {
        PageAdapter<UserEntity> page = userService.listPage(currentPage, size);
        return Result.success(page);
    }

    @PostMapping("/delete")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> page(@RequestBody List<Long> ids) {
        userService.deleteUsers(ids);
        return Result.success();
    }

    @GetMapping("/info/{id}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<UserEntity> info(@PathVariable(value = "id") Long id) {
        UserEntity user = userService.findByIdWithoutPassword(id);
        return Result.success(user);
    }

}
