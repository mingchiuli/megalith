package org.chiu.megalith.manage.controller;

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

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;

import java.util.List;

@RestController
@RequestMapping(value = "/sys/user")
@RequiredArgsConstructor
@Validated
public class UserController {

    private final UserService userService;

    private final RoleService roleService;

    @PostMapping("/save")
    @PreAuthorize("hasAuthority('sys:user:save')")
    public Result<Void> save(@RequestBody @Valid UserEntityReq userEntityReq) {
        return Result.success(() -> userService.saveOrUpdate(userEntityReq));
    }

    @GetMapping("/page/{currentPage}")
    @PreAuthorize("hasAuthority('sys:user:page')")
    public Result<PageAdapter<UserEntityVo>> page(@PathVariable(value = "currentPage") Integer currentPage,
                                                  @RequestParam(value = "size", defaultValue = "5") Integer size) {
        return Result.success(() -> userService.listPage(currentPage, size));
    }

    @PostMapping("/delete")
    @PreAuthorize("hasAuthority('sys:user:delete')")
    public Result<Void> delete(@RequestBody @NotEmpty List<Long> ids) {
        return Result.success(() -> userService.deleteUsers(ids));
    }

    @GetMapping("/info/{id}")
    @PreAuthorize("hasAuthority('sys:user:info')")
    public Result<UserEntityVo> info(@PathVariable(value = "id") Long id) {
        return Result.success(() -> userService.findById(id));
    }

    @GetMapping("/role/valid/all")
    @PreAuthorize("hasAuthority('sys:user:role:valid:all')")
    public Result<List<RoleEntityVo>> getValidAll() {
        return Result.success(roleService::getValidAll);
    }

}
