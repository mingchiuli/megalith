package org.chiu.megalith.search.controller;

import jakarta.validation.Valid;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.search.service.WebsiteSearchService;
import org.chiu.megalith.search.vo.WebsiteDocumentVo;
import org.chiu.megalith.search.req.WebsiteDocumentReq;
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
@Validated
public class WebsiteSearchController {

    private final WebsiteSearchService websiteSearchService;

    @PostMapping("/save")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> save(@RequestBody @Valid WebsiteDocumentReq websiteDocumentReq) {
        return Result.success(() -> websiteSearchService.saveOrUpdate(websiteDocumentReq));
    }

    @GetMapping("/delete/{id}")
    @PreAuthorize("hasRole(@highestRoleHolder.getRole())")
    public Result<Void> delete(@PathVariable(value = "id") String id) {
        return Result.success(() -> websiteSearchService.delete(id));
    }

    @GetMapping("/{currentPage}")
    public Result<PageAdapter<WebsiteDocumentVo>> search(@PathVariable Integer currentPage,
                                                         @RequestParam(required = false) String keyword,
                                                         @RequestParam(required = false, defaultValue = "9") Integer pageSize) {
        return Result.success(() -> websiteSearchService.search(currentPage, keyword, pageSize));
    }

    @GetMapping("/info/{id}")
    public Result<WebsiteDocumentVo> searchById(@PathVariable String id) {
        return Result.success(() -> websiteSearchService.searchById(id));
    }
}
