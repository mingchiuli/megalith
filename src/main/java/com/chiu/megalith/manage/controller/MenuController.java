package com.chiu.megalith.manage.controller;

import com.chiu.megalith.manage.entity.MenuEntity;
import com.chiu.megalith.manage.service.MenuService;
import com.chiu.megalith.manage.vo.MenuEntityVo;
import com.chiu.megalith.common.jwt.JwtUtils;
import com.chiu.megalith.common.lang.Result;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtException;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * @author mingchiuli
 * @create 2022-12-04 2:22 am
 */
@RestController
@RequestMapping("/sys/menu")
@RequiredArgsConstructor
public class MenuController {

    private final MenuService menuService;

    private final JwtUtils jwtUtils;

    @GetMapping("/nav")
    public Result<List<MenuEntityVo>> nav(HttpServletRequest request) {
        String jwt = request.getHeader(HttpHeaders.AUTHORIZATION);
        Claims claim = jwtUtils.getClaimByToken(jwt).orElseThrow(() -> new JwtException("invalid token"));;
        Long userId = Long.parseLong(claim.getSubject());
        List<MenuEntityVo> navs = menuService.getCurrentUserNav(userId);
        return Result.success(navs);
    }

    @GetMapping("/info/{id}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<MenuEntity> info(@PathVariable(name = "id") Long id) {
        MenuEntity menu = menuService.findById(id);
        return Result.success(menu);
    }

    @GetMapping("/list")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<List<MenuEntityVo>> list() {
        List<MenuEntityVo> menus = menuService.tree();
        return Result.success(menus);
    }

    @PostMapping("/save")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> saveOrUpdate(@Validated @RequestBody MenuEntityVo menu) {
        menuService.saveOrUpdate(menu);
        return Result.success();
    }

    @PostMapping("/delete/{id}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> delete(@PathVariable("id") Long id) {
        menuService.delete(id);
        return Result.success();
    }

}
