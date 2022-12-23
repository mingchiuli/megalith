package com.chiu.megalith.search.controller;

import com.chiu.megalith.blog.dto.BlogEntityDto;
import com.chiu.megalith.common.lang.Result;
import com.chiu.megalith.common.page.PageAdapter;
import com.chiu.megalith.common.valid.ListValue;
import com.chiu.megalith.search.service.BlogSearchService;
import com.chiu.megalith.search.vo.BlogDocumentVo;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

/**
 * @author mingchiuli
 * @create 2022-11-30 8:48 pm
 */
@RestController
@RequestMapping(value = "/search/blog")
@RequiredArgsConstructor
public class BlogSearchController {

    private final BlogSearchService blogSearchService;

    @GetMapping("/{flag}/{currentPage}")
    public Result<PageAdapter<BlogDocumentVo>> selectBlogsByES(@PathVariable Integer currentPage,
                                                               @PathVariable @ListValue(values = {0, 1}, message = "must commit 0 or 1") Integer flag ,
                                                               @RequestParam(value = "year", required = false) Integer year,
                                                               @RequestParam(value = "keyword") String keyword) {
        PageAdapter<BlogDocumentVo> page = blogSearchService.selectBlogsByES(currentPage, keyword, flag, year);
        return Result.success(page);
    }

    @PreAuthorize("hasAnyRole(@highestRoleHolder.getRole(), @defaultRoleHolder.getRole())")
    @GetMapping("/sys/blogs")
    public Result<PageAdapter<BlogEntityDto>> searchAllBlogs(@RequestParam(value = "keyword") String keyword,
                                                             @RequestParam(defaultValue = "1") Integer currentPage,
                                                             @RequestParam(defaultValue = "5") Integer size) {
        PageAdapter<BlogEntityDto> page = blogSearchService.searchAllBlogs(keyword, currentPage, size);
        return Result.success(page);
    }

}
