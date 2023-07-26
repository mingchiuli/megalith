package org.chiu.megalith.search.controller;

import org.chiu.megalith.blog.dto.BlogEntityDto;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.search.service.BlogSearchService;
import org.chiu.megalith.search.vo.BlogDocumentVo;
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

    @GetMapping("/blog")
    public Result<PageAdapter<BlogDocumentVo>> selectBlogsByES(@RequestParam(value = "currentPage", defaultValue = "-1") Integer currentPage,
                                                               @RequestParam(value = "allInfo") Boolean allInfo,
                                                               @RequestParam(value = "year", required = false) String year,
                                                               @RequestParam(value = "keywords") @NotBlank String keywords) {
        return Result.success(() -> blogSearchService.selectBlogsByES(currentPage, keywords, allInfo, year));
    }

    @GetMapping("/sys/blogs")
    public Result<PageAdapter<BlogEntityDto>> searchAllBlogs(@RequestParam(defaultValue = "1") Integer currentPage,
                                                             @RequestParam(defaultValue = "5") Integer size,
                                                             @RequestParam(value = "keyword")  @NotBlank String keywords) {
        return Result.success(() -> blogSearchService.searchAllBlogs(keywords, currentPage, size));
    }

}