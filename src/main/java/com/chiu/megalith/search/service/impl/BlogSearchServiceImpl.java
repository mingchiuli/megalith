package com.chiu.megalith.search.service.impl;

import co.elastic.clients.elasticsearch._types.SortOrder;
import com.chiu.megalith.infra.utils.ESHighlightBuilderUtils;
import com.chiu.megalith.exhibit.dto.BlogEntityDto;
import com.chiu.megalith.infra.page.PageAdapter;
import com.chiu.megalith.search.document.BlogDocument;
import com.chiu.megalith.search.service.BlogSearchService;
import com.chiu.megalith.search.vo.BlogDocumentVo;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.client.elc.NativeQuery;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.query.HighlightQuery;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-11-30 9:00 pm
 */
@Service
@RequiredArgsConstructor
public class BlogSearchServiceImpl implements BlogSearchService {

    private final ElasticsearchTemplate elasticsearchTemplate;

    @Value("${blog.blog-page-size}")
    private int blogPageSize;

    private final List<String> fields = List.of("title", "description", "content");

    @Override
    public PageAdapter<BlogDocumentVo> selectBlogsByES(Integer currentPage,
                                                       String keyword,
                                                       Integer flag,
                                                       Integer year) {
        HighlightQuery highlightQuery;

        if (flag == 0) {
            highlightQuery = ESHighlightBuilderUtils.blogHighlightQueryOrigin;
        } else {
            highlightQuery = ESHighlightBuilderUtils.blogHighlightQuerySimple;
        }

        NativeQuery matchQuery = NativeQuery
                .builder()
                .withQuery(query ->
                        query.bool(boolQuery ->
                                boolQuery
                                        .must(mustQuery1 ->
                                                mustQuery1.multiMatch(multiQuery ->
                                                        multiQuery.fields(fields).query(keyword)))
                                        .must(mustQuery2 ->
                                                mustQuery2.term(termQuery ->
                                                        termQuery.field("status").value(0)))
                                        .must(mustQuery3 ->
                                                mustQuery3.range(rangeQuery ->
                                                        rangeQuery.field("created")
                                                                .from(Optional.ofNullable(year).isPresent() ? year + "-01-01T00:00:00.000" : null)
                                                                .to(Optional.ofNullable(year).isPresent() ? year + "-12-31T23:59:59.999" : null)))))
                .withSort(sort ->
                        sort.score(score ->
                                score.order(SortOrder.Desc)))
                .withPageable(PageRequest.of(currentPage - 1, blogPageSize))
                .withHighlightQuery(highlightQuery)
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
                            .created(document.getCreated())
                            .score(hit.getScore())
                            .highlight(hit.getHighlightFields().values())
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
    public PageAdapter<BlogEntityDto> searchAllBlogs(String keyword,
                                                     Integer currentPage,
                                                     Integer size) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        NativeQuery nativeQuery = NativeQuery
                .builder()
                .withQuery(query ->
                        query.bool(boolQuery ->
                                boolQuery
                                        .must(mustQuery1 ->
                                                mustQuery1.multiMatch(multiQuery -> multiQuery.
                                                        fields(fields).query(keyword)))
                                        .must(mustQuery2 ->
                                                mustQuery2.term(termQuery ->
                                                        termQuery.field("userId").value(userId)))))
                .withPageable(PageRequest.of(currentPage - 1, size))
                .withSort(sortQuery ->
                        sortQuery.field(fieldQuery ->
                                fieldQuery.field("created").order(SortOrder.Desc)))
                .build();

        SearchHits<BlogDocument> search = elasticsearchTemplate.search(nativeQuery, BlogDocument.class);
        long totalHits = search.getTotalHits();
        long totalPage = totalHits % size == 0 ? totalHits / size : totalHits / size + 1;

        List<BlogEntityDto> entities = search.getSearchHits().stream()
                .map(hit -> {
                    BlogDocument document = hit.getContent();
                    return BlogEntityDto.builder()
                            .id(document.getId())
                            .title(document.getTitle())
                            .description(document.getDescription())
                            .content(document.getContent())
                            .created(document.getCreated().toLocalDateTime())
                            .status(document.getStatus())
                            .build();
                })
                .toList();

        return PageAdapter.<BlogEntityDto>builder()
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
