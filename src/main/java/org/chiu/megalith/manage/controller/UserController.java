package org.chiu.megalith.manage.controller;

import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.service.UserService;
import org.chiu.megalith.manage.vo.UserEntityVo;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.infra.page.PageAdapter;
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
        return Result.success(() -> userService.saveOrUpdate(userEntityVo));
    }

    @GetMapping("/status/{id}/{status}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> setUserStatus(@PathVariable(value = "id") Long userId,
                                         @PathVariable(value = "status") Integer status) {
        return Result.success(() -> userService.changeUserStatusById(userId, status));
    }

    @GetMapping("/page/{currentPage}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<PageAdapter<UserEntity>> page(@PathVariable(value = "currentPage") Integer currentPage,
                                                @RequestParam(value = "size", defaultValue = "5") Integer size) {
        return Result.success(() -> userService.listPage(currentPage, size));
    }

    @PostMapping("/delete")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> page(@RequestBody List<Long> ids) {
        return Result.success(() -> userService.deleteUsers(ids));
    }

    @GetMapping("/info/{id}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<UserEntity> info(@PathVariable(value = "id") Long id) {
        UserEntity user = userService.findById(id);
        user.setPassword(null);
        return Result.success(user);
    }

}
