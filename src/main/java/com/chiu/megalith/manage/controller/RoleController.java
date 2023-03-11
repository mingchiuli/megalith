package com.chiu.megalith.manage.controller;

import com.chiu.megalith.manage.entity.RoleEntity;
import com.chiu.megalith.manage.service.RoleService;
import com.chiu.megalith.manage.vo.RoleEntityVo;
import com.chiu.megalith.base.lang.Result;
import com.chiu.megalith.base.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-06 8:28 pm
 */
@RestController
@RequestMapping(value = "/sys/role")
@RequiredArgsConstructor
public class RoleController {

    private final RoleService roleService;

    @GetMapping("/info/{id}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<RoleEntity> info(@PathVariable("id") Long id) {
        RoleEntity role = roleService.info(id);
        return Result.success(role);
    }

    @GetMapping("/page")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public Result<PageAdapter<RoleEntity>> listPage(@RequestParam(defaultValue = "1") Integer currentPage,
                                                    @RequestParam(defaultValue = "5") Integer pageSize) {
        PageAdapter<RoleEntity> pageData = roleService.listPage(currentPage, pageSize);
        return Result.success(pageData);
    }

    @PostMapping("/save")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> saveOrUpdate(@Validated @RequestBody RoleEntityVo role) {
        roleService.saveOrUpdate(role);
        return Result.success();
    }

    @PostMapping("/delete")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> delete(@RequestBody List<Long> ids) {
        roleService.delete(ids);
        return Result.success();
    }

    @PostMapping("/perm/{roleId}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<List<Long>> info(@PathVariable("roleId") Long roleId,
                                   @RequestBody List<Long> menuIds) {
        menuIds = roleService.perm(roleId, menuIds);
        return Result.success(menuIds);
    }


}
