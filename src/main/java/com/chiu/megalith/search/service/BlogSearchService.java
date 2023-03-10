package com.chiu.megalith.search.service;


import com.chiu.megalith.exhibit.dto.BlogEntityDto;
import com.chiu.megalith.base.page.PageAdapter;
import com.chiu.megalith.search.vo.BlogDocumentVo;

/**
 * @author mingchiuli
 * @create 2022-11-30 8:52 pm
 */
public interface BlogSearchService {

    PageAdapter<BlogDocumentVo> selectBlogsByES(Integer currentPage, String keyword, Integer flag, Integer year);

    PageAdapter<BlogEntityDto> searchAllBlogs(String keyword, Integer currentPage, Integer size);
}
