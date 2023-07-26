package org.chiu.megalith.manage.controller;

import org.chiu.megalith.manage.entity.MenuEntity;
import org.chiu.megalith.manage.service.MenuService;
import org.chiu.megalith.manage.vo.MenuEntityVo;
import org.chiu.megalith.infra.lang.Result;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
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

    @GetMapping("/nav")
    public Result<List<MenuEntityVo>> nav() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        Long userId = Long.valueOf(authentication.getName());
        return Result.success(() -> menuService.getCurrentUserNav(userId));
    }

    @GetMapping("/info/{id}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<MenuEntity> info(@PathVariable(name = "id") Long id) {
        return Result.success(() -> menuService.findById(id));
    }

    @GetMapping("/list")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<List<MenuEntityVo>> list() {
        return Result.success(menuService::tree);
    }

    @PostMapping("/save")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> saveOrUpdate(@Validated @RequestBody MenuEntityVo menu) {
        return Result.success(() -> menuService.saveOrUpdate(menu));
    }

    @PostMapping("/delete/{id}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> delete(@PathVariable("id") Long id) {
        return Result.success(() -> menuService.delete(id));
    }

}