package com.chiu.megalith.search.service.impl;

import co.elastic.clients.elasticsearch._types.SortOrder;
import co.elastic.clients.elasticsearch._types.query_dsl.BoolQuery;
import co.elastic.clients.elasticsearch._types.query_dsl.Query;
import com.chiu.megalith.infra.page.PageAdapter;
import com.chiu.megalith.infra.utils.ESHighlightBuilderUtils;
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
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.time.ZonedDateTime;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-12-23 5:44 pm
 */
@Service
@RequiredArgsConstructor
public class WebsiteSearchServiceImpl implements WebsiteSearchService {

    private final ElasticsearchTemplate elasticsearchTemplate;

    @Value("${blog.web-page-size}")
    private int webPageSize;

    @Value("${blog.highest-role}")
    private String highestRole;

    private final List<String> fields = List.of("title", "description");

    @Override
    public void saveOrUpdate(WebsiteVo websiteVo) {
        var ref = new Object() {
            WebsiteDocument document;
        };

        Optional.ofNullable(websiteVo.getId()).ifPresentOrElse(id ->
                        ref.document = elasticsearchTemplate.get(id, WebsiteDocument.class),
                () -> ref.document = WebsiteDocument.builder()
                        .created(ZonedDateTime.now())
                        .build());

        BeanUtils.copyProperties(websiteVo, ref.document);
        elasticsearchTemplate.save(ref.document);
    }

    @Override
    public void delete(String id) {
        elasticsearchTemplate.delete(id, WebsiteDocument.class);
    }

    @Override
    public PageAdapter<WebsiteDocumentVo> search(Integer currentPage,
                                                 String keyword) {
        var ref = new Object() {
            boolean auth = false;
        };

        Optional.ofNullable(SecurityContextHolder.getContext().getAuthentication()).ifPresent(authentication -> {
            String authority = authentication.getAuthorities().stream()
                    .findFirst()
                    .map(GrantedAuthority::getAuthority)
                    .orElseThrow();

            if (("ROLE_" + highestRole).equals(authority)) {
                ref.auth = true;
            }
        });

        NativeQueryBuilder nativeQueryBuilder = NativeQuery.builder()
                .withPageable(PageRequest.of(currentPage - 1, webPageSize));

        Query.Builder builder = new Query.Builder();
        BoolQuery.Builder boolBuilder = new BoolQuery.Builder();


        if (!ref.auth) {
            boolBuilder.must(mustQuery ->
                    mustQuery.term(termQuery ->
                            termQuery.field("status").value(0)));
        }

        Optional.ofNullable(keyword).ifPresentOrElse(word -> {
            boolBuilder.must(mustQuery ->
                    mustQuery.multiMatch(multiQuery ->
                            multiQuery.fields(fields).query(keyword)));
            nativeQueryBuilder
                    .withSort(sort ->
                            sort.score(score ->
                                    score.order(SortOrder.Desc)))
                    .withHighlightQuery(ESHighlightBuilderUtils.websiteHighlightQuery);
        }, () -> nativeQueryBuilder
                .withSort(sortQuery ->
                        sortQuery.field(fieldQuery ->
                                fieldQuery.field("created").order(SortOrder.Desc))));

        NativeQuery matchQuery = nativeQueryBuilder
                .withQuery(builder
                        .bool(boolBuilder
                                .build())
                        .build())
                .build();

        SearchHits<WebsiteDocument> search = elasticsearchTemplate.search(matchQuery, WebsiteDocument.class);
        long totalHits = search.getTotalHits();
        long totalPage = totalHits % webPageSize == 0 ? totalHits / webPageSize : totalHits / webPageSize + 1;

        List<WebsiteDocumentVo> vos = search.getSearchHits().stream()
                .map(hit -> {
                    WebsiteDocument document = hit.getContent();
                    Collection<List<String>> highlightValues = hit.getHighlightFields().values();
                    float score = hit.getScore();
                    return WebsiteDocumentVo.builder()
                            .id(document.getId())
                            .title(document.getTitle())
                            .description(document.getDescription())
                            .link(document.getLink())
                            .status(document.getStatus())
                            .created(document.getCreated())
                            .highlight(!highlightValues.isEmpty() ?
                                    highlightValues :
                                    null)
                            .score(!Float.isNaN(score) ?
                                    score :
                                    null)
                            .build();
                })
                .toList();

        return PageAdapter.<WebsiteDocumentVo>builder()
                .first(currentPage == 1)
                .last(currentPage == totalPage)
                .pageSize(webPageSize)
                .pageNumber(currentPage)
                .empty(totalHits == 0)
                .totalElements(totalHits)
                .totalPages((int) totalPage)
                .content(vos)
                .build();
    }
}
