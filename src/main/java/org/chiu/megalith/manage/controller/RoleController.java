package org.chiu.megalith.manage.controller;

import org.chiu.megalith.manage.service.RoleMenuService;
import org.chiu.megalith.manage.service.RoleService;
import org.chiu.megalith.manage.req.RoleEntityReq;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.infra.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.vo.MenuRoleVo;
import org.chiu.megalith.manage.vo.RoleEntityVo;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-06 8:28 pm
 */
@RestController
@RequestMapping(value = "/sys/role")
@RequiredArgsConstructor
@Validated
public class RoleController {

    private final RoleService roleService;

    private final RoleMenuService roleMenuService;

    @GetMapping("/info/{id}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<RoleEntityVo> info(@PathVariable("id") Long id) {
        return Result.success(() -> roleService.info(id));
    }

    @GetMapping("/roles")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<PageAdapter<RoleEntityVo>> getPage(@RequestParam(defaultValue = "1") Integer currentPage,
                                                     @RequestParam(defaultValue = "5") Integer size) {
        return Result.success(() -> roleService.getPage(currentPage, size));
    }

    @PostMapping("/save")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> saveOrUpdate(@RequestBody @Valid RoleEntityReq role) {
        return Result.success(() -> roleService.saveOrUpdate(role));
    }

    @PostMapping("/delete")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> delete(@RequestBody @NotEmpty List<Long> ids) {
        return Result.success(() -> roleService.delete(ids));
    }

    @PostMapping("/perm/{roleId}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<List<Long>> savePerm(@PathVariable("roleId") Long roleId,
                                       @RequestBody @NotEmpty List<Long> menuIds) {
        return Result.success(() -> roleService.savePerm(roleId, menuIds));
    }

    @GetMapping("/perm/{roleId}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<List<MenuRoleVo>> getMenusInfo(@PathVariable Long roleId) {
        return Result.success(() -> roleMenuService.getMenusInfo(roleId));
    }
}
