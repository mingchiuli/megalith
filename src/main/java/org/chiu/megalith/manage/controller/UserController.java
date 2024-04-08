package org.chiu.megalith.manage.controller;

import org.chiu.megalith.manage.req.UserEntityRegisterReq;
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
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

@RestController
@RequestMapping(value = "/sys/user")
@RequiredArgsConstructor
@Validated
public class UserController {

    private final UserService userService;

    private final RoleService roleService;

    @GetMapping("/auth/register/page")
    @PreAuthorize("hasAuthority('sys:user:register:page')")
    public Result<String> getRegisterPage() {
        return Result.success(userService::getRegisterPage);
    }

    @GetMapping("/register/check")
    public Result<Boolean> checkRegisterPage(String token) {
        return Result.success(() -> userService.checkRegisterPage(token));
    }


    @PostMapping("/register/save")
    public Result<Void> saveRegisterPage(@RequestParam String token, @RequestBody @Valid UserEntityRegisterReq userEntityRegisterReq) {
        return Result.success(() -> userService.saveRegisterPage(token, userEntityRegisterReq));
    }

    @PostMapping("/register/image/upload")
    public Result<String> imageUpload(@RequestParam MultipartFile image, @RequestParam String token) {
        return Result.success(() -> userService.imageUpload(token, image));
    }

    @GetMapping("/register/image/delete")
    public Result<Void> imageDelete(@RequestParam String url, @RequestParam String token) {
        return Result.success(() -> userService.imageDelete(token, url));
    }

    @PostMapping("/save")
    @PreAuthorize("hasAuthority('sys:user:save')")
    public Result<Void> saveOrUpdate(@RequestBody @Valid UserEntityReq userEntityReq) {
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
