package com.chiu.megalith.search.service;

import com.chiu.megalith.search.vo.WebsiteVo;

/**
 * @author mingchiuli
 * @create 2022-12-23 5:44 pm
 */
public interface WebsiteSearchService {
    String generateJwt();

    void saveOrUpdate(WebsiteVo document);

}
