package org.chiu.megalith.coop.controller;

import org.chiu.megalith.coop.vo.BlogAbstractVo;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.blog.vo.BlogEntityVo;
import org.chiu.megalith.infra.lang.Result;
import org.chiu.megalith.infra.valid.CoopBlogId;
import org.chiu.megalith.coop.service.CoopService;
import org.chiu.megalith.coop.vo.InitCoopVo;
import lombok.RequiredArgsConstructor;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;


/**
 * @author mingchiuli
 * @create 2022-12-25 7:14 pm
 */
@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("/coop")
public class CoopController {
    private final CoopService coopService;

    @GetMapping("/init/{blogId}/{orderNumber}")
    public Result<InitCoopVo> initCoopBlog(@PathVariable @CoopBlogId Long blogId,
                                           @PathVariable Integer orderNumber) {
        return Result.success(() -> coopService.joinCoopBlog(blogId, orderNumber));
    }

    @GetMapping("/blogs/{currentPage}")
    public Result<PageAdapter<BlogAbstractVo>> getCoopBlogs(@PathVariable Integer currentPage) {
        return Result.success(() -> coopService.getCoopBlogs(currentPage));
    }

    @PostMapping("/save/{blogId}")
    public Result<Void> submitBlog(@PathVariable @CoopBlogId Long blogId,
                                   @RequestBody @Validated BlogEntityVo blogEntityVo) {
        return Result.success(() -> coopService.submitBlog(blogId, blogEntityVo));
    }
}
