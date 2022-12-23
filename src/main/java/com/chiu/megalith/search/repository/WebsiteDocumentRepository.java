package com.chiu.megalith.search.repository;

import com.chiu.megalith.search.document.WebsiteDocument;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;
import org.springframework.stereotype.Repository;

/**
 * @author mingchiuli
 * @create 2022-12-23 3:17 pm
 */
@Repository
@SuppressWarnings("unused")
public interface WebsiteDocumentRepository extends ElasticsearchRepository<WebsiteDocument, String> {}
