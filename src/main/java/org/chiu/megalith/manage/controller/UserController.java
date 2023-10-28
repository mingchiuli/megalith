package org.chiu.megalith.manage.controller;

import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.service.RoleService;
import org.chiu.megalith.manage.service.UserService;
import org.chiu.megalith.manage.req.UserEntityReq;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.infra.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.vo.RoleEntityVo;
import org.chiu.megalith.manage.vo.UserEntityVo;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping(value = "/sys/user")
@RequiredArgsConstructor
public class UserController {

    private final UserService userService;

    private final RoleService roleService;

    @PostMapping("/save")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> save(@Validated @RequestBody UserEntityReq userEntityReq) {
        return Result.success(() -> userService.saveOrUpdate(userEntityReq));
    }

    @GetMapping("/page/{currentPage}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<PageAdapter<UserEntityVo>> page(@PathVariable(value = "currentPage") Integer currentPage,
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
    public Result<UserEntityVo> info(@PathVariable(value = "id") Long id) {
        UserEntity user = userService.findById(id);
        return Result.success(UserEntityVo.builder()
                .email(user.getEmail())
                .phone(user.getPhone())
                .role(user.getRole())
                .id(user.getId())
                .nickname(user.getNickname())
                .status(user.getStatus())
                .avatar(user.getAvatar())
                .created(user.getCreated())
                .lastLogin(user.getLastLogin())
                .username(user.getUsername())
                .build());
    }

    @GetMapping("/role/valid/all")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<List<RoleEntityVo>> getValidAll() {
        return Result.success(roleService::getValidAll);
    }

}
