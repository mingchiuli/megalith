package com.chiu.megalith.search.repository;

import com.chiu.megalith.search.document.BlogDocument;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

/**
 * 只为自动建立索引
 * @author mingchiuli
 * @create 2022-12-23 2:11 pm
 */
@ConditionalOnProperty(value = "spring.profiles.active", havingValue = "prod")
public interface BlogDocumentRepository extends ElasticsearchRepository<BlogDocument, Long> {}
