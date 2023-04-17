package com.chiu.megalith.coop.service;

import com.chiu.megalith.coop.vo.BlogAbstractVo;
import com.chiu.megalith.infra.page.PageAdapter;
import com.chiu.megalith.blog.vo.BlogEntityVo;
import com.chiu.megalith.coop.vo.InitCoopVo;

/**
 * @author mingchiuli
 * @create 2022-12-26 1:05 am
 */
public interface CoopService {
    InitCoopVo joinCoopBlog(Long blogId, Integer orderNumber);

    void submitBlog(Long blogId, BlogEntityVo blogEntityVo);

    PageAdapter<BlogAbstractVo> getCoopBlogs(Integer currentPage);
}
