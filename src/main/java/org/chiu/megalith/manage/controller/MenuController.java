package org.chiu.megalith.manage.controller;

import org.chiu.megalith.infra.utils.SecurityUtils;
import org.chiu.megalith.manage.service.MenuService;
import org.chiu.megalith.manage.req.MenuEntityReq;
import org.chiu.megalith.infra.lang.Result;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.service.RoleMenuService;
import org.chiu.megalith.manage.vo.MenuEntityVo;
import org.chiu.megalith.manage.vo.MenuVo;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * @author mingchiuli
 * @create 2022-12-04 2:22 am
 */
@RestController
@RequestMapping(value = "/sys/menu")
@RequiredArgsConstructor
public class MenuController {

    private final MenuService menuService;

    private final RoleMenuService roleMenuService;

    @GetMapping("/nav")
    public Result<List<MenuVo>> nav() {
        Long userId = SecurityUtils.getLoginUserId();
        return Result.success(() -> roleMenuService.getCurrentUserNav(userId));
    }

    @GetMapping("/info/{id}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<MenuEntityVo> info(@PathVariable(name = "id") Long id) {
        return Result.success(() -> menuService.findById(id));
    }

    @GetMapping("/list")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<List<MenuVo>> list() {
        return Result.success(roleMenuService::tree);
    }

    @PostMapping("/save")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> saveOrUpdate(@Validated @RequestBody MenuEntityReq menu) {
        return Result.success(() -> menuService.saveOrUpdate(menu));
    }

    @PostMapping("/delete/{id}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> delete(@PathVariable("id") Long id) {
        return Result.success(() -> menuService.delete(id));
    }

}
