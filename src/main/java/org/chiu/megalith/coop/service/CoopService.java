package org.chiu.megalith.coop.service;

import org.chiu.megalith.coop.vo.BlogAbstractVo;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.blog.vo.BlogEntityVo;
import org.chiu.megalith.coop.vo.InitCoopVo;

/**
 * @author mingchiuli
 * @create 2022-12-26 1:05 am
 */
public interface CoopService {
    
    InitCoopVo initCoopSession(Long blogId, Integer orderNumber);

    void submitBlog(Long blogId, BlogEntityVo blogEntityVo);

    PageAdapter<BlogAbstractVo> getCoopBlogsInfo(Integer currentPage);
}
