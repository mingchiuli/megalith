package org.chiu.megalith.manage.controller;

import jakarta.servlet.http.HttpServletResponse;
import org.chiu.megalith.manage.service.RoleAuthorityService;
import org.chiu.megalith.manage.service.RoleMenuService;
import org.chiu.megalith.manage.service.RoleService;
import org.chiu.megalith.manage.req.RoleEntityReq;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.infra.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.vo.RoleAuthorityVo;
import org.chiu.megalith.manage.vo.RoleMenuVo;
import org.chiu.megalith.manage.vo.RoleEntityVo;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;

import java.util.ArrayList;
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

    private final RoleAuthorityService roleAuthorityService;

    @GetMapping("/info/{id}")
    @PreAuthorize("hasAuthority('sys:role:info')")
    public Result<RoleEntityVo> info(@PathVariable("id") Long id) {
        return Result.success(() -> roleService.info(id));
    }

    @GetMapping("/roles")
    @PreAuthorize("hasAuthority('sys:role:roles')")
    public Result<PageAdapter<RoleEntityVo>> getPage(@RequestParam(defaultValue = "1") Integer currentPage,
                                                     @RequestParam(defaultValue = "5") Integer size) {
        return Result.success(() -> roleService.getPage(currentPage, size));
    }

    @PostMapping("/save")
    @PreAuthorize("hasAuthority('sys:role:save')")
    public Result<Void> saveOrUpdate(@RequestBody @Valid RoleEntityReq role) {
        return Result.success(() -> roleService.saveOrUpdate(role));
    }

    @PostMapping("/delete")
    @PreAuthorize("hasAuthority('sys:role:delete')")
    public Result<Void> delete(@RequestBody @NotEmpty List<Long> ids) {
        return Result.success(() -> roleService.delete(ids));
    }

    @PostMapping("/menu/{roleId}")
    @PreAuthorize("hasAuthority('sys:role:menu:save')")
    public Result<Void> saveMenu(@PathVariable("roleId") Long roleId,
                                 @RequestBody ArrayList<Long> menuIds) {
        return Result.success(() -> roleMenuService.saveMenu(roleId, menuIds));
    }

    @GetMapping("/menu/{roleId}")
    @PreAuthorize("hasAuthority('sys:role:menu:get')")
    public Result<List<RoleMenuVo>> getMenusInfo(@PathVariable Long roleId) {
        return Result.success(() -> roleMenuService.getMenusInfo(roleId));
    }

    @PostMapping("/authority/{roleId}")
    @PreAuthorize("hasAuthority('sys:role:authority:save')")
    public Result<Void> saveAuthority(@PathVariable("roleId") Long roleId,
                                      @RequestBody ArrayList<Long> authorityIds) {
        return Result.success(() -> roleAuthorityService.saveAuthority(roleId, authorityIds));
    }

    @GetMapping("/authority/{roleId}")
    @PreAuthorize("hasAuthority('sys:role:authority:get')")
    public Result<List<RoleAuthorityVo>> getAuthoritiesInfo(@PathVariable Long roleId) {
        return Result.success(() -> roleAuthorityService.getAuthoritiesInfo(roleId));
    }

    @GetMapping("/download")
    @PreAuthorize("hasAuthority('sys:role:download')")
    public Result<Void> download(HttpServletResponse response) {
        roleService.download(response);
        return Result.success();
    }
}
