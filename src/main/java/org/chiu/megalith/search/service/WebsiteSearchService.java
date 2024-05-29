package org.chiu.megalith.search.service;

import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.search.vo.WebsiteDocumentVo;
import org.chiu.megalith.search.req.WebsiteDocumentReq;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-23 5:44 pm
 */
public interface WebsiteSearchService {

    void saveOrUpdate(WebsiteDocumentReq document);

    void delete(String id);

    PageAdapter<WebsiteDocumentVo> search(Integer currentPage, String keyword, Integer pageSize, List<String> roles);

    WebsiteDocumentVo searchById(String id, List<String> roles);
}
