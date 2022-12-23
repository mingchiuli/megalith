package com.chiu.megalith.search.repository;

import com.chiu.megalith.search.document.BlogDocument;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;
import org.springframework.stereotype.Repository;

/**
 * 只为自动建立索引
 * @author mingchiuli
 * @create 2022-12-23 2:11 pm
 */
@Repository
@SuppressWarnings("unused")
public interface BlogDocumentRepository extends ElasticsearchRepository<BlogDocument, Long> {}
