package org.chiu.megalith.search.controller;

import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.search.service.WebsiteSearchService;
import org.chiu.megalith.search.vo.WebsiteDocumentVo;
import org.chiu.megalith.search.vo.WebsiteVo;
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

    @PostMapping("/save")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
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

    @GetMapping("/{currentPage}")
    public Result<PageAdapter<WebsiteDocumentVo>> search(@PathVariable Integer currentPage,
                                                         @RequestParam(required = false) String keyword) {
        PageAdapter<WebsiteDocumentVo> page = websiteSearchService.search(currentPage, keyword);
        return Result.success(page);
    }
}
