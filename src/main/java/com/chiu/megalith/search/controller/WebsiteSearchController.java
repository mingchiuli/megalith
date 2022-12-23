package com.chiu.megalith.search.controller;

import com.chiu.megalith.blog.cache.Cache;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.lang.Result;
import com.chiu.megalith.search.service.WebsiteSearchService;
import com.chiu.megalith.search.vo.WebsiteVo;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * @author mingchiuli
 * @create 2022-12-23 3:15 pm
 */
@RestController
@RequestMapping(value = "/search/website")
@RequiredArgsConstructor
public class WebsiteSearchController {

    private final WebsiteSearchService websiteSearchService;

    @GetMapping("/jwt/generate")
    @Cache(prefix = Const.JSON_WEB_TOKEN)
    public Result<String> generateJwt() {
        String jwt = websiteSearchService.generateJwt();
        return Result.success(jwt);
    }

    @PostMapping("/save")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole(), @jwtRoleHolder.getRole())")
    public Result<Void> saveOrUpdate(@Validated @RequestBody WebsiteVo websiteVo) {
        websiteSearchService.saveOrUpdate(websiteVo);
        return Result.success();
    }
}
