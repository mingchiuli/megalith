package org.chiu.megalith.manage.controller;

import org.chiu.megalith.manage.entity.RoleEntity;
import org.chiu.megalith.manage.service.RoleService;
import org.chiu.megalith.manage.vo.RoleEntityVo;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.infra.page.PageAdapter;
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
        return Result.success(() -> roleService.info(id));
    }

    @GetMapping("/page")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<PageAdapter<RoleEntity>> getPage(@RequestParam(defaultValue = "1") Integer currentPage,
                                                    @RequestParam(defaultValue = "5") Integer pageSize) {
        return Result.success(() -> roleService.getPage(currentPage, pageSize));
    }

    @PostMapping("/save")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> saveOrUpdate(@Validated @RequestBody RoleEntityVo role) {
        return Result.success(() -> roleService.saveOrUpdate(role));
    }

    @PostMapping("/delete")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> delete(@RequestBody List<Long> ids) {
        return Result.success(() -> roleService.delete(ids));
    }

    @PostMapping("/perm/{roleId}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<List<Long>> info(@PathVariable("roleId") Long roleId,
                                   @RequestBody List<Long> menuIds) {
        return Result.success(() -> roleService.perm(roleId, menuIds));
    }


}
