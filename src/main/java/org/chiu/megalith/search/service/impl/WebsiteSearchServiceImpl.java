package org.chiu.megalith.search.service.impl;

import co.elastic.clients.elasticsearch._types.SortOrder;
import co.elastic.clients.elasticsearch._types.query_dsl.BoolQuery;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.infra.utils.ESHighlightBuilderUtils;
import org.chiu.megalith.infra.utils.SecurityUtils;
import org.chiu.megalith.search.convertor.WebsiteDocumentVoConvertor;
import org.chiu.megalith.search.document.WebsiteDocument;
import org.chiu.megalith.search.service.WebsiteSearchService;
import org.chiu.megalith.search.vo.WebsiteDocumentVo;
import org.chiu.megalith.search.req.WebsiteDocumentReq;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.BeanUtils;
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

import static org.chiu.megalith.manage.lang.FieldEnum.*;
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

    private final SecurityUtils securityUtils;

    private final List<String> fields = List.of(TITLE.getField(), DESCRIPTION.getField() + "^2");

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
            String role = SecurityUtils.getLoginRole();
            if (securityUtils.isAdmin(role)) {
                auth = true;
            }
        }

        var nativeQueryBuilder = NativeQuery.builder()
                .withPageable(PageRequest.of(currentPage - 1, pageSize));

        var boolBuilder = new BoolQuery.Builder();

        if (Boolean.FALSE.equals(auth)) {
            boolBuilder.filter(filterQuery -> filterQuery
                    .term(termQuery -> termQuery
                            .field(STATUS.getField())
                            .value(StatusEnum.NORMAL.getCode())));
        }

        if (StringUtils.hasLength(keyword)) {
            boolBuilder.must(mustQuery -> mustQuery
                    .multiMatch(multiQuery -> multiQuery
                            .fields(fields)
                            .query(keyword)));
            nativeQueryBuilder
                    .withSort(sort -> sort
                            .score(score -> score.order(SortOrder.Desc)))
                    .withHighlightQuery(ESHighlightBuilderUtils.websiteHighlightQuery);
        } else {
            nativeQueryBuilder.withSort(sortQuery -> sortQuery
                    .field(fieldQuery -> fieldQuery
                            .field(CREATED.getField())
                            .order(SortOrder.Desc)));
        }

        var matchQuery = nativeQueryBuilder
                .withQuery(qry -> qry
                        .bool(boolBuilder.build()))
                .build();

        SearchHits<WebsiteDocument> search = elasticsearchTemplate.search(matchQuery, WebsiteDocument.class);
        return WebsiteDocumentVoConvertor.convert(search, pageSize, currentPage);
    }

    @Override
    public WebsiteDocumentVo searchById(String id) {
        WebsiteDocument document = elasticsearchTemplate.get(id, WebsiteDocument.class);
        if (Objects.isNull(document)) {
            throw new MissException(DOCUMENT_NOT_EXIST);
        }

        int status = document.getStatus();
        String role = SecurityUtils.getLoginRole();
        if (status == StatusEnum.HIDE.getCode() && !securityUtils.isAdmin(role)) {
            throw new MissException(DOCUMENT_NOT_EXIST);
        }

        return WebsiteDocumentVoConvertor.convert(document);
    }
}
