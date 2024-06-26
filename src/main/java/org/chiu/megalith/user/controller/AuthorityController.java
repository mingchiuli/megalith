package org.chiu.megalith.user.controller;

import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.user.req.AuthorityEntityReq;
import org.chiu.megalith.user.service.AuthorityService;
import org.chiu.megalith.user.vo.AuthorityVo;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping(value = "/sys/authority")
@RequiredArgsConstructor
@Validated
public class AuthorityController {

    private final AuthorityService authorityService;

    @GetMapping("/list")
    @PreAuthorize("hasAuthority('sys:authority:list')")
    public Result<List<AuthorityVo>> list() {
        return Result.success(authorityService::findAll);
    }


    @GetMapping("/info/{id}")
    @PreAuthorize("hasAuthority('sys:authority:info')")
    public Result<AuthorityVo> info(@PathVariable(value = "id") Long id) {
        return Result.success(() -> authorityService.findById(id));
    }

    @PostMapping("/save")
    @PreAuthorize("hasAuthority('sys:authority:save')")
    public Result<Void> saveOrUpdate(@RequestBody @Valid AuthorityEntityReq req) {
        return Result.success(() -> authorityService.saveOrUpdate(req));
    }

    @PostMapping("/delete")
    @PreAuthorize("hasAuthority('sys:authority:delete')")
    public Result<Void> delete(@RequestBody @NotEmpty List<Long> ids) {
        return Result.success(() -> authorityService.deleteAuthorities(ids));
    }

    @GetMapping("/download")
    @PreAuthorize("hasAuthority('sys:authority:download')")
    public Result<Void> download(HttpServletResponse response) {
        authorityService.download(response);
        return Result.success();
    }
}
