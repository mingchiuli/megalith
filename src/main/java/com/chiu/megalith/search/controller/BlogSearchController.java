package com.chiu.megalith.search.controller;

import com.chiu.megalith.blog.dto.BlogEntityDto;
import com.chiu.megalith.infra.lang.Result;
import com.chiu.megalith.infra.page.PageAdapter;
import com.chiu.megalith.infra.valid.ListValue;
import com.chiu.megalith.search.service.BlogSearchService;
import com.chiu.megalith.search.vo.BlogDocumentVo;
import jakarta.validation.constraints.NotBlank;
import lombok.RequiredArgsConstructor;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * @author mingchiuli
 * @create 2022-11-30 8:48 pm
 */
@RestController
@RequestMapping(value = "/search")
@RequiredArgsConstructor
@Validated
public class BlogSearchController {

    private final BlogSearchService blogSearchService;

    @GetMapping("/blog/{flag}/{currentPage}")
    public Result<PageAdapter<BlogDocumentVo>> selectBlogsByES(@PathVariable Integer currentPage,
                                                               @PathVariable @ListValue(values = {0, 1}, message = "must commit 0 or 1") Integer flag,
                                                               @RequestParam(value = "year", required = false) Integer year,
                                                               @RequestParam(value = "keyword") @NotBlank String keyword) {
        PageAdapter<BlogDocumentVo> page = blogSearchService.selectBlogsByES(currentPage, keyword, flag, year);
        return Result.success(page);
    }

    @GetMapping("/sys/blogs")
    public Result<PageAdapter<BlogEntityDto>> searchAllBlogs(@RequestParam(defaultValue = "1") Integer currentPage,
                                                             @RequestParam(defaultValue = "5") Integer size,
                                                             @RequestParam(value = "keyword")  @NotBlank String keyword) {
        PageAdapter<BlogEntityDto> page = blogSearchService.searchAllBlogs(keyword, currentPage, size);
        return Result.success(page);
    }

}
