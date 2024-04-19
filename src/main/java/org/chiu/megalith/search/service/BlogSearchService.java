package org.chiu.megalith.search.service;


import org.chiu.megalith.manage.vo.BlogEntityVo;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.search.vo.BlogDocumentVo;

/**
 * @author mingchiuli
 * @create 2022-11-30 8:52 pm
 */
public interface BlogSearchService {

    PageAdapter<BlogDocumentVo> selectBlogsByES(Integer currentPage, String keywords, Boolean allInfo, String year);

    PageAdapter<BlogEntityVo> searchAllBlogs(String keywords, Integer currentPage, Integer size, Long userId);
}
