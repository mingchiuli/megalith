package com.chiu.megalith.search.service.impl;

import co.elastic.clients.elasticsearch._types.SortOrder;
import com.chiu.megalith.blog.dto.BlogEntityDto;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.page.PageAdapter;
import com.chiu.megalith.search.document.BlogDocument;
import com.chiu.megalith.search.service.BlogSearchService;
import com.chiu.megalith.search.vo.BlogDocumentVo;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.client.elc.NativeQuery;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.query.HighlightQuery;
import org.springframework.data.elasticsearch.core.query.highlight.Highlight;
import org.springframework.data.elasticsearch.core.query.highlight.HighlightField;
import org.springframework.data.elasticsearch.core.query.highlight.HighlightParameters;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.util.Arrays;
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

    private final StringRedisTemplate redisTemplate;

    private final int blogPageSize = Integer.parseInt(Const.BLOG_PAGE_SIZE.getMsg());

    @Override
    public PageAdapter<BlogDocumentVo> selectBlogsByES(Integer currentPage, String keyword, Integer flag, Integer year) {

        NativeQuery matchQuery = NativeQuery.builder().
                withQuery(query -> query.
                        bool(boolQuery -> boolQuery.
                                must(mustQuery1 -> mustQuery1.
                                        multiMatch(multiQuery -> multiQuery.
                                                fields(Arrays.asList("title", "description", "content")).query(keyword))).
                                must(mustQuery2 -> mustQuery2.
                                        term(termQuery -> termQuery.
                                                field("status").value(0))).
                                must(mustQuery3 -> mustQuery3.
                                        range(rangeQuery -> rangeQuery.
                                                field("created").
                                                from(year != null ? year + "-01-01T00:00:00.000" : null).
                                                to(year != null ? year + "-12-31T23:59:59.999" : null)))))
                .withSort(sort -> sort.
                        score(score -> score.
                                order(SortOrder.Desc)))
                .withPageable(PageRequest.of(currentPage - 1, blogPageSize))
                .withHighlightQuery(new HighlightQuery(
                        new Highlight(flag == 0 ?
                                new HighlightParameters.
                                        HighlightParametersBuilder().
                                        withPreTags("<b style='color:red'>").
                                        withPostTags("</b>").
                                        withNumberOfFragments(1).
                                        withFragmentSize(5).
                                        build() :
                                new HighlightParameters.
                                        HighlightParametersBuilder().
                                        withPreTags("<b style='color:red'>").
                                        withPostTags("</b>").
                                        build(),
                                Arrays.asList(
                                        new HighlightField("title"),
                                        new HighlightField("description"),
                                        new HighlightField("content"))),
                        null)).
                build();

        SearchHits<BlogDocument> search = elasticsearchTemplate.search(matchQuery, BlogDocument.class);
        long totalHits = search.getTotalHits();
        long totalPage = totalHits % blogPageSize == 0 ? totalHits / blogPageSize : totalHits / blogPageSize + 1;

        List<BlogDocumentVo> vos = search.getSearchHits().
                stream().
                map(hit ->
                        BlogDocumentVo.builder().
                                id(hit.getContent().
                                        getId()).
                                userId(hit.getContent().
                                        getUserId()).
                                status(hit.getContent().
                                        getStatus()).
                                title(hit.getContent().
                                        getTitle()).
                                description(hit.getContent().
                                        getDescription()).
                                content(hit.getContent().
                                        getContent()).
                                link(hit.getContent().
                                        getLink()).
                                created(hit.getContent().
                                        getCreated()).
                                score(hit.getScore()).
                                highlight(hit.getHighlightFields().values().toString()).
                                build()).
                toList();

        return PageAdapter.
                <BlogDocumentVo>builder().
                first(currentPage == 1).
                last(currentPage == totalPage).
                pageSize(blogPageSize).
                pageNumber(currentPage).
                empty(totalHits == 0).
                totalElements(totalHits).
                totalPages((int) totalPage).
                content(vos).
                build();
    }

    @Override
    public PageAdapter<BlogEntityDto> searchAllBlogs(String keyword, Integer currentPage, Integer size) {
        long userId = Long.parseLong(SecurityContextHolder.getContext().getAuthentication().getName());

        NativeQuery nativeQuery = NativeQuery.builder().
                withQuery(query -> query.
                        bool(boolQuery -> boolQuery.
                                must(mustQuery1 -> mustQuery1.
                                        multiMatch(multiQuery -> multiQuery.
                                                fields(Arrays.asList("title", "description", "content")).query(keyword))).
                                must(mustQuery2 -> mustQuery2.
                                        term(termQuery -> termQuery.
                                                field("userId").value(userId))))).
                withPageable(PageRequest.of(currentPage - 1, size)).
                withSort(sortQuery -> sortQuery.
                        field(fieldQuery -> fieldQuery.
                                field("created").order(SortOrder.Desc))).
                build();

        SearchHits<BlogDocument> search = elasticsearchTemplate.search(nativeQuery, BlogDocument.class);
        long totalHits = search.getTotalHits();
        long totalPage = totalHits % size == 0 ? totalHits / size : totalHits / size + 1;

        List<BlogEntityDto> entities = search.getSearchHits().
                stream().
                map(hit ->
                        BlogEntityDto.builder().
                                id(hit.getContent().
                                        getId()).
                                title(hit.getContent().
                                        getTitle()).
                                description(hit.getContent().
                                        getDescription()).
                                content(hit.getContent().
                                        getContent()).
                                created(hit.getContent().
                                        getCreated().toLocalDateTime()).
                                status(hit.getContent().
                                        getStatus()).
                                readRecent(Integer.valueOf(
                                    Optional.ofNullable(redisTemplate.opsForValue().get(
                                            Const.READ_RECENT.getMsg() + hit.getContent().getId()
                                            )).
                                            orElse("0")
                                )).
                                build()
                ).
                toList();

        return PageAdapter.<BlogEntityDto>builder().
                totalElements(totalHits).
                pageNumber(currentPage).
                pageSize(size).
                empty(totalHits == 0).
                first(currentPage == 1).
                last(currentPage == totalPage).
                totalPages((int) totalPage).
                content(entities).
                build();
    }

}
