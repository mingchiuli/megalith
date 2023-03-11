package com.chiu.megalith.search.service;

import com.chiu.megalith.base.page.PageAdapter;
import com.chiu.megalith.search.vo.WebsiteDocumentVo;
import com.chiu.megalith.search.vo.WebsiteVo;

/**
 * @author mingchiuli
 * @create 2022-12-23 5:44 pm
 */
public interface WebsiteSearchService {
    String generateJwt();

    void saveOrUpdate(WebsiteVo document);

    void delete(String id);

    PageAdapter<WebsiteDocumentVo> authSearch(Integer currentPage, String keyword);

    PageAdapter<WebsiteDocumentVo> search(Integer currentPage, String keyword);
}
