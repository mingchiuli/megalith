package com.chiu.megalith.search.service;


import com.chiu.megalith.blog.dto.BlogEntityDto;
import com.chiu.megalith.infra.page.PageAdapter;
import com.chiu.megalith.search.vo.BlogDocumentVo;

/**
 * @author mingchiuli
 * @create 2022-11-30 8:52 pm
 */
public interface BlogSearchService {

    PageAdapter<BlogDocumentVo> selectBlogsByES(Integer currentPage, String keywords, Boolean allInfo, String year);

    PageAdapter<BlogEntityDto> searchAllBlogs(String keywords, Integer currentPage, Integer size);
}
