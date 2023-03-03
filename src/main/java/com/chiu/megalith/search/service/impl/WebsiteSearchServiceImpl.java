package com.chiu.megalith.search.service.impl;

import co.elastic.clients.elasticsearch._types.SortOrder;
import com.chiu.megalith.common.jwt.JwtUtils;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.page.PageAdapter;
import com.chiu.megalith.search.document.WebsiteDocument;
import com.chiu.megalith.search.service.WebsiteSearchService;
import com.chiu.megalith.search.vo.WebsiteDocumentVo;
import com.chiu.megalith.search.vo.WebsiteVo;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.client.elc.NativeQuery;
import org.springframework.data.elasticsearch.client.elc.NativeQueryBuilder;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.query.HighlightQuery;
import org.springframework.data.elasticsearch.core.query.highlight.Highlight;
import org.springframework.data.elasticsearch.core.query.highlight.HighlightField;
import org.springframework.data.elasticsearch.core.query.highlight.HighlightParameters;
import org.springframework.stereotype.Service;

import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-12-23 5:44 pm
 */
@Service
@RequiredArgsConstructor
public class WebsiteSearchServiceImpl implements WebsiteSearchService {

    private final JwtUtils jwtUtils;

    private final ElasticsearchTemplate elasticsearchTemplate;

    @Value("${blog.web-page-size}")
    private int webPageSize;

    @Override
    public String generateJwt() {
        return jwtUtils.generateToken(Const.ROLE_TOKEN_TOOL.getInfo(), Const.ROLE_TOKEN_TOOL.getInfo());
    }

    @Override
    public void saveOrUpdate(WebsiteVo websiteVo) {
        var ref = new Object() {
            WebsiteDocument document;
        };

        Optional.ofNullable(websiteVo.getId()).ifPresentOrElse(id ->
                ref.document = elasticsearchTemplate.get(id, WebsiteDocument.class),
                () ->
                        ref.document = WebsiteDocument.builder().
                                created(ZonedDateTime.now()).
                                build());

        BeanUtils.copyProperties(websiteVo, ref.document);
        elasticsearchTemplate.save(ref.document);
    }

    @Override
    public void delete(String id) {
        elasticsearchTemplate.delete(id, WebsiteDocument.class);
    }

    @Override
    public PageAdapter<WebsiteDocumentVo> authSearch(Integer currentPage,
                                                     String keyword) {

        NativeQuery matchQuery = NativeQuery.
                builder().
                withQuery(query ->
                        query.bool(boolQuery ->
                                boolQuery.must(mustQuery ->
                                        mustQuery.multiMatch(multiQuery ->
                                                multiQuery.fields(Arrays.asList("title", "description")).query(keyword))))).
                withSort(sort ->
                        sort.score(score ->
                                score.order(SortOrder.Desc))).
                withPageable(PageRequest.of(currentPage - 1, webPageSize)).
                withHighlightQuery(
                        new HighlightQuery(
                                new Highlight(
                                        new HighlightParameters.
                                                HighlightParametersBuilder().
                                                withPreTags("<b style='color:red'>").
                                                withPostTags("</b>").
                                                build(),
                                        Arrays.asList(
                                                new HighlightField("title"),
                                                new HighlightField("description"))
                                ), null)
                ).
                build();

        SearchHits<WebsiteDocument> search = elasticsearchTemplate.search(matchQuery, WebsiteDocument.class);
        long totalHits = search.getTotalHits();
        long totalPage = totalHits % webPageSize == 0 ? totalHits / webPageSize : totalHits / webPageSize + 1;

        List<WebsiteDocumentVo> vos = search.getSearchHits().
                stream().
                map(hit -> {
                    WebsiteDocument document = hit.getContent();
                    return WebsiteDocumentVo.
                            builder().
                            id(document.getId()).
                            title(document.getTitle()).
                            description(document.getDescription()).
                            link(document.getLink()).
                            status(document.getStatus()).
                            created(document.getCreated()).
                            highlight(hit.getHighlightFields().values()).
                            score(hit.getScore()).
                            build();
                }).
                toList();

        return PageAdapter.
                <WebsiteDocumentVo>builder().
                first(currentPage == 1).
                last(currentPage == totalPage).
                pageSize(webPageSize).
                pageNumber(currentPage).
                empty(totalHits == 0).
                totalElements(totalHits).
                totalPages((int) totalPage).
                content(vos).
                build();
    }

    @Override
    public PageAdapter<WebsiteDocumentVo> search(Integer currentPage,
                                                 String keyword) {

        NativeQueryBuilder nativeQueryBuilder = NativeQuery.builder()
                .withPageable(PageRequest.of(currentPage - 1, webPageSize));

        Optional.ofNullable(keyword).ifPresentOrElse(word ->
                nativeQueryBuilder.
                        withSort(sort ->
                                sort.score(score ->
                                        score.order(SortOrder.Desc))).
                        withHighlightQuery(
                                new HighlightQuery(
                                        new Highlight(
                                                new HighlightParameters.
                                                        HighlightParametersBuilder().
                                                        withPreTags("<b style='color:red'>").
                                                        withPostTags("</b>").
                                                        build(),
                                                Arrays.asList(
                                                        new HighlightField("title"),
                                                        new HighlightField("description"))
                                        ), null)).
                        withQuery(query ->
                                query.bool(boolQuery ->
                                        boolQuery.
                                                must(mustQuery1 ->
                                                        mustQuery1.multiMatch(multiQuery ->
                                                                multiQuery.fields(Arrays.asList("title", "description")).query(keyword))).
                                                must(mustQuery2 ->
                                                        mustQuery2.term(termQuery ->
                                                                termQuery.field("status").value(0))))), () ->
                nativeQueryBuilder.
                        withSort(sortQuery ->
                                sortQuery.field(fieldQuery ->
                                        fieldQuery.field("created").order(SortOrder.Desc))).
                        withQuery(query ->
                                query.bool(boolQuery ->
                                        boolQuery.must(mustQuery2 ->
                                                mustQuery2.term(termQuery ->
                                                        termQuery.field("status").value(0))))));

        NativeQuery matchQuery = nativeQueryBuilder.build();

        SearchHits<WebsiteDocument> search = elasticsearchTemplate.search(matchQuery, WebsiteDocument.class);
        long totalHits = search.getTotalHits();
        long totalPage = totalHits % webPageSize == 0 ? totalHits / webPageSize : totalHits / webPageSize + 1;


        List<WebsiteDocumentVo> vos = search.getSearchHits().
                stream().
                map(hit -> {
                    WebsiteDocument document = hit.getContent();
                    return WebsiteDocumentVo.
                            builder().
                            id(document.getId()).
                            title(document.getTitle()).
                            description(document.getDescription()).
                            link(document.getLink()).
                            status(document.getStatus()).
                            created(document.getCreated()).
                            highlight(!hit.getHighlightFields().values().isEmpty() ?
                                    hit.getHighlightFields().values() :
                                    null).
                            score(!Float.isNaN(hit.getScore()) ?
                                    hit.getScore() :
                                    null).
                            build();
                }).
                toList();

        return PageAdapter.
                <WebsiteDocumentVo>builder().
                first(currentPage == 1).
                last(currentPage == totalPage).
                pageSize(webPageSize).
                pageNumber(currentPage).
                empty(totalHits == 0).
                totalElements(totalHits).
                totalPages((int) totalPage).
                content(vos).
                build();
    }
}
