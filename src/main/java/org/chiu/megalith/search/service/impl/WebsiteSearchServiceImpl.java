package org.chiu.megalith.search.service.impl;

import co.elastic.clients.elasticsearch._types.SortOrder;
import co.elastic.clients.elasticsearch._types.query_dsl.BoolQuery;
import co.elastic.clients.elasticsearch._types.query_dsl.Query;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.infra.utils.ESHighlightBuilderUtils;
import org.chiu.megalith.infra.utils.SecurityUtils;
import org.chiu.megalith.search.document.WebsiteDocument;
import org.chiu.megalith.search.service.WebsiteSearchService;
import org.chiu.megalith.search.vo.WebsiteDocumentVo;
import org.chiu.megalith.search.req.WebsiteDocumentReq;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.client.elc.NativeQuery;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.Objects;

import static org.chiu.megalith.infra.lang.Const.ROLE_PREFIX;
import static org.chiu.megalith.infra.lang.ExceptionMessage.DOCUMENT_NOT_EXIST;
import static org.chiu.megalith.infra.lang.ExceptionMessage.WEB_NOT_EXIST;

/**
 * @author mingchiuli
 * @create 2022-12-23 5:44 pm
 */
@Service
@RequiredArgsConstructor
public class WebsiteSearchServiceImpl implements WebsiteSearchService {

    private final ElasticsearchTemplate elasticsearchTemplate;

    @Value("${blog.highest-role}")
    private String highestRole;

    private final List<String> fields = List.of("title", "description^2");

    @Override
    public void saveOrUpdate(WebsiteDocumentReq websiteDocumentReq) {
        String id = websiteDocumentReq.getId();
        WebsiteDocument document;
        var now = ZonedDateTime.now();

        if (Objects.nonNull(id)) {
            document = elasticsearchTemplate.get(id, WebsiteDocument.class);
            if (Objects.isNull(document)) {
                throw new MissException(WEB_NOT_EXIST);
            }
        } else {
            document = WebsiteDocument.builder()
                    .created(now)
                    .build();
        }
        
        document.setUpdated(now);
        BeanUtils.copyProperties(websiteDocumentReq, document);
        elasticsearchTemplate.save(document);
    }

    @Override
    public void delete(String id) {
        elasticsearchTemplate.delete(id, WebsiteDocument.class);
    }

    @Override
    public PageAdapter<WebsiteDocumentVo> search(Integer currentPage, String keyword, Integer pageSize) {

        Authentication authentication = SecurityUtils.getLoginAuthentication();
        boolean auth = false;

        if (Objects.nonNull(authentication)) {
            String authority = SecurityUtils.getLoginAuthority();
            if ((ROLE_PREFIX.getInfo() + highestRole).equals(authority)) {
                auth = true;
            }
        }

        var nativeQueryBuilder = NativeQuery.builder()
                .withPageable(PageRequest.of(currentPage - 1, pageSize));

        var boolBuilder = new BoolQuery.Builder();

        if (Boolean.FALSE.equals(auth)) {
            boolBuilder.filter(filterQuery ->
                    filterQuery.term(termQuery ->
                            termQuery.field("status").value(StatusEnum.NORMAL.getCode())));
        }

        if (StringUtils.hasLength(keyword)) {
            boolBuilder.must(mustQuery ->
                    mustQuery.multiMatch(multiQuery ->
                            multiQuery.fields(fields).query(keyword)));
            nativeQueryBuilder
                    .withSort(sort ->
                            sort.score(score ->
                                    score.order(SortOrder.Desc)))
                    .withHighlightQuery(ESHighlightBuilderUtils.websiteHighlightQuery);
        } else {
            nativeQueryBuilder
                    .withSort(sortQuery ->
                            sortQuery.field(fieldQuery ->
                                    fieldQuery.field("created").order(SortOrder.Desc)));
        }

        var matchQuery = nativeQueryBuilder
                .withQuery(new Query.Builder()
                        .bool(boolBuilder
                                .build())
                        .build())
                .build();

        SearchHits<WebsiteDocument> search = elasticsearchTemplate.search(matchQuery, WebsiteDocument.class);
        long totalHits = search.getTotalHits();
        long totalPage = totalHits % pageSize == 0 ? totalHits / pageSize : totalHits / pageSize + 1;

        List<WebsiteDocumentVo> vos = search.getSearchHits().stream()
                .map(hit -> {
                    WebsiteDocument document = hit.getContent();
                    return WebsiteDocumentVo.builder()
                            .id(document.getId())
                            .title(document.getTitle())
                            .description(document.getDescription())
                            .link(document.getLink())
                            .status(document.getStatus())
                            .created(document.getCreated())
                            .updated(document.getUpdated())
                            .highlight(hit.getHighlightFields())
                            .score(hit.getScore())
                            .build();
                })
                .toList();

        return PageAdapter.<WebsiteDocumentVo>builder()
                .first(currentPage == 1)
                .last(currentPage == totalPage)
                .pageSize(pageSize)
                .pageNumber(currentPage)
                .empty(totalHits == 0)
                .totalElements(totalHits)
                .totalPages((int) totalPage)
                .content(vos)
                .build();
    }

    @Override
    public WebsiteDocumentVo searchById(String id) {
        WebsiteDocument document = elasticsearchTemplate.get(id, WebsiteDocument.class);
        if (Objects.isNull(document)) {
            throw new MissException(DOCUMENT_NOT_EXIST);
        }

        int status = document.getStatus();
        String authority = SecurityUtils.getLoginAuthority();
        if (status == StatusEnum.HIDE.getCode() && !(ROLE_PREFIX.getInfo() + highestRole).equals(authority)) {
            throw new MissException(DOCUMENT_NOT_EXIST);
        }

        return WebsiteDocumentVo.builder()
                .id(document.getId())
                .title(document.getTitle())
                .description(document.getDescription())
                .link(document.getLink())
                .status(document.getStatus())
                .created(document.getCreated())
                .build();
    }
}
