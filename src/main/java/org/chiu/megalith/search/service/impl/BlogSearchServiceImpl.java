package org.chiu.megalith.search.service.impl;

import co.elastic.clients.elasticsearch._types.SortOrder;
import org.chiu.megalith.blog.service.BlogService;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.utils.ESHighlightBuilderUtils;
import org.chiu.megalith.blog.vo.BlogEntityVo;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.search.document.BlogDocument;
import org.chiu.megalith.search.service.BlogSearchService;
import org.chiu.megalith.search.vo.BlogDocumentVo;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.client.elc.NativeQuery;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-11-30 9:00 pm
 */
@Service
@RequiredArgsConstructor
public class BlogSearchServiceImpl implements BlogSearchService {

    private final ElasticsearchTemplate elasticsearchTemplate;

    private final StringRedisTemplate redisTemplate;

    private final BlogService blogService;

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

    private final List<String> fields = List.of("title", "description", "content^2");

    @Override
    public PageAdapter<BlogDocumentVo> selectBlogsByES(Integer currentPage, String keywords, Boolean allInfo, String year) {
        var matchQuery = NativeQuery.builder()
                .withQuery(query ->
                        query.bool(boolQuery ->
                                boolQuery
                                        .must(mustQuery1 ->
                                                mustQuery1.multiMatch(multiQuery ->
                                                        multiQuery.fields(fields).query(keywords)))
                                        .filter(filterQuery2 ->
                                                filterQuery2.term(termQuery ->
                                                        termQuery.field("status").value(0)))
                                        .filter(filterQuery3 ->
                                                filterQuery3.range(rangeQuery ->
                                                        rangeQuery.field("created")
                                                                .from(StringUtils.hasLength(year) ? year + "-01-01T00:00:00.000" : null)
                                                                .to(StringUtils.hasLength(year) ? year + "-12-31T23:59:59.999" : null)))))
                .withSort(sort ->
                        sort.score(score ->
                                score.order(SortOrder.Desc)))
                .withPageable(Objects.equals(currentPage, -1) ? 
                        PageRequest.of(0, 10) : 
                        PageRequest.of(currentPage - 1, blogPageSize))
                .withHighlightQuery(Boolean.TRUE.equals(allInfo) ? 
                        ESHighlightBuilderUtils.blogHighlightQueryOrigin : 
                        ESHighlightBuilderUtils.blogHighlightQuerySimple)
                .build();

        SearchHits<BlogDocument> search = elasticsearchTemplate.search(matchQuery, BlogDocument.class);
        long totalHits = search.getTotalHits();
        long totalPage = totalHits % blogPageSize == 0 ? totalHits / blogPageSize : totalHits / blogPageSize + 1;

        List<BlogDocumentVo> vos = search.getSearchHits().stream()
                .map(hit -> {
                    BlogDocument document = hit.getContent();
                    return BlogDocumentVo.builder()
                            .id(document.getId())
                            .userId(document.getUserId())
                            .status(document.getStatus())
                            .title(document.getTitle())
                            .description(document.getDescription())
                            .content(document.getContent())
                            .link(document.getLink())
                            .created(document.getCreated().toLocalDateTime())
                            .score(hit.getScore())
                            .highlight(hit.getHighlightFields())
                            .build();
                })
                .toList();

        return PageAdapter.<BlogDocumentVo>builder()
                .first(currentPage == 1)
                .last(currentPage == totalPage)
                .pageSize(blogPageSize).pageNumber(currentPage)
                .empty(totalHits == 0)
                .totalElements(totalHits)
                .totalPages((int) totalPage)
                .content(vos)
                .build();
    }

    @Override
    public PageAdapter<BlogEntityVo> searchAllBlogs(String keywords, Integer currentPage, Integer size, Long userId) {

        var nativeQuery = NativeQuery.builder()
                .withQuery(query ->
                        query.bool(boolQuery ->
                                boolQuery
                                        .must(mustQuery1 ->
                                                mustQuery1.multiMatch(multiQuery -> multiQuery.
                                                        fields(fields).query(keywords)))
                                        .filter(filterQuery2 ->
                                                filterQuery2.term(termQuery ->
                                                        termQuery.field("userId").value(userId)))))
                .withPageable(PageRequest.of(currentPage - 1, size))
                .withSort(sortQuery ->
                        sortQuery.score(score ->
                                score.order(SortOrder.Desc)))
                .build();

        SearchHits<BlogDocument> search = elasticsearchTemplate.search(nativeQuery, BlogDocument.class);
        long totalHits = search.getTotalHits();
        long totalPage = totalHits % size == 0 ? totalHits / size : totalHits / size + 1;

        List<BlogEntityVo> entities = search.getSearchHits().stream()
                .map(hit -> {
                    BlogDocument document = hit.getContent();
                    Long id = document.getId();
                    Long readCount;
                    try {
                        readCount = blogService.findById(id, false).getReadCount();
                    } catch (MissException e) {
                        readCount = blogService.findById(id, true).getReadCount();
                    }
                    return BlogEntityVo.builder()
                            .id(id)
                            .title(document.getTitle())
                            .description(document.getDescription())
                            .content(document.getContent())
                            .readCount(readCount)
                            .recentReadCount(Optional.ofNullable(redisTemplate.opsForZSet().score(Const.HOT_READ.getInfo(), document.getId().toString()))
                                    .orElse(0.0))
                            .created(document.getCreated().toLocalDateTime())
                            .updated(document.getUpdated().toLocalDateTime())
                            .status(document.getStatus())
                            .build();
                })
                .toList();

        return PageAdapter.<BlogEntityVo>builder()
                .totalElements(totalHits)
                .pageNumber(currentPage)
                .pageSize(size)
                .empty(totalHits == 0)
                .first(currentPage == 1)
                .last(currentPage == totalPage)
                .totalPages((int) totalPage)
                .content(entities)
                .build();
    }

}
