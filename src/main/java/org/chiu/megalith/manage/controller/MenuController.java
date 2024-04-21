package org.chiu.megalith.manage.controller;

import jakarta.servlet.http.HttpServletResponse;
import org.chiu.megalith.infra.utils.SecurityUtils;
import org.chiu.megalith.manage.service.MenuService;
import org.chiu.megalith.manage.req.MenuEntityReq;
import org.chiu.megalith.infra.lang.Result;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.service.RoleMenuService;
import org.chiu.megalith.manage.vo.MenuDisplayVo;
import org.chiu.megalith.manage.vo.MenuEntityVo;
import org.chiu.megalith.manage.vo.MenusAndButtons;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;

import java.util.List;


/**
 * @author mingchiuli
 * @create 2022-12-04 2:22 am
 */
@RestController
@RequestMapping(value = "/sys/menu")
@RequiredArgsConstructor
@Validated
public class MenuController {

    private final MenuService menuService;

    private final RoleMenuService roleMenuService;

    @GetMapping("/nav")
    @PreAuthorize("hasAuthority('sys:menu:nav')")
    public Result<MenusAndButtons> nav() {
        String role = SecurityUtils.getLoginRole();
        return Result.success(() -> roleMenuService.getCurrentUserNav(role));
    }

    @GetMapping("/info/{id}")
    @PreAuthorize("hasAuthority('sys:menu:info')")
    public Result<MenuEntityVo> info(@PathVariable(name = "id") Long id) {
        return Result.success(() -> menuService.findById(id));
    }

    @GetMapping("/list")
    @PreAuthorize("hasAuthority('sys:menu:list')")
    public Result<List<MenuDisplayVo>> list() {
        return Result.success(menuService::tree);
    }

    @PostMapping("/save")
    @PreAuthorize("hasAuthority('sys:menu:save')")
    public Result<Void> saveOrUpdate(@RequestBody @Valid MenuEntityReq menu) {
        return Result.success(() -> menuService.saveOrUpdate(menu));
    }

    @PostMapping("/delete/{id}")
    @PreAuthorize("hasAuthority('sys:menu:delete')")
    public Result<Void> delete(@PathVariable("id") Long id) {
        return Result.success(() -> menuService.delete(id));
    }

    @GetMapping("/download")
    @PreAuthorize("hasAuthority('sys:menu:download')")
    public Result<Void> download(HttpServletResponse response) {
        menuService.download(response);
        return Result.success();
    }

}
