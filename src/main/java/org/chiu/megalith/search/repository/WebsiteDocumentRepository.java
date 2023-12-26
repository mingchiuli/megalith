package org.chiu.megalith.search.repository;

import org.chiu.megalith.search.document.WebsiteDocument;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

/**
 * @author mingchiuli
 * @create 2022-12-23 3:17 pm
 */
public interface WebsiteDocumentRepository extends ElasticsearchRepository<WebsiteDocument, String> {}
