package com.chiu.megalith.search.service;

import com.chiu.megalith.infra.page.PageAdapter;
import com.chiu.megalith.search.vo.WebsiteDocumentVo;
import com.chiu.megalith.search.vo.WebsiteVo;

/**
 * @author mingchiuli
 * @create 2022-12-23 5:44 pm
 */
public interface WebsiteSearchService {

    void saveOrUpdate(WebsiteVo document);

    void delete(String id);

    PageAdapter<WebsiteDocumentVo> search(Integer currentPage, String keyword);
}
