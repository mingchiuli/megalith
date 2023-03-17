package com.chiu.megalith.search.controller;

import com.chiu.megalith.exhibit.cache.Cache;
import com.chiu.megalith.base.lang.Const;
import com.chiu.megalith.base.lang.Result;
import com.chiu.megalith.base.page.PageAdapter;
import com.chiu.megalith.search.service.WebsiteSearchService;
import com.chiu.megalith.search.vo.WebsiteDocumentVo;
import com.chiu.megalith.search.vo.WebsiteVo;
import jakarta.validation.constraints.NotBlank;
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

    @GetMapping("/token")
    @Cache(prefix = Const.JSON_WEB_TOKEN)
    public Result<String> generateToken() {
        String jwt = websiteSearchService.generateJwt();
        return Result.success(jwt);
    }

    @PostMapping("/save")
    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    public Result<Void> save(@Validated @RequestBody WebsiteVo websiteVo) {
        websiteSearchService.saveOrUpdate(websiteVo);
        return Result.success();
    }

    @GetMapping("/delete/{id}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> delete(@PathVariable(value = "id") String id) {
        websiteSearchService.delete(id);
        return Result.success();
    }


    @GetMapping("/auth/{currentPage}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<PageAdapter<WebsiteDocumentVo>> authSearch(@PathVariable Integer currentPage,
                                                             @RequestParam @NotBlank String keyword) {
        PageAdapter<WebsiteDocumentVo> page = websiteSearchService.authSearch(currentPage, keyword);
        return Result.success(page);
    }

    @GetMapping("/{currentPage}")
    public Result<PageAdapter<WebsiteDocumentVo>> search(@PathVariable Integer currentPage,
                                                         @RequestParam(required = false) String keyword) {
        PageAdapter<WebsiteDocumentVo> page = websiteSearchService.search(currentPage, keyword);
        return Result.success(page);
    }
}
