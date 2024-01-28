package org.chiu.megalith.search.service.impl;

import co.elastic.clients.elasticsearch._types.SortOrder;
import co.elastic.clients.elasticsearch._types.query_dsl.FunctionBoostMode;
import co.elastic.clients.elasticsearch._types.query_dsl.FunctionScoreMode;

import org.chiu.megalith.blog.convertor.BlogEntityVoConvertor;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.infra.utils.ESHighlightBuilderUtils;
import org.chiu.megalith.blog.vo.BlogEntityVo;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.search.convertor.BlogDocumentVoConvertor;
import org.chiu.megalith.search.document.BlogDocument;
import org.chiu.megalith.search.service.BlogSearchService;
import org.chiu.megalith.search.vo.BlogDocumentVo;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.client.elc.NativeQuery;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Objects;

import static org.chiu.megalith.blog.lang.FieldEnum.*;

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

    private final List<String> fields = List.of(TITLE.getField(), DESCRIPTION.getField(), CONTENT.getField());

    @Override
    public PageAdapter<BlogDocumentVo> selectBlogsByES(Integer currentPage, String keywords, Boolean allInfo, String year) {

        var matchQuery = NativeQuery.builder()
                .withQuery(query -> query
                        .functionScore(functionScore -> functionScore
                                .query(baseQuery -> baseQuery
                                        //做高亮必須在query里搜高亮字段
                                        //不做高亮就不用写
                                        .bool(boolQry -> boolQry
                                                .must(mustQry -> mustQry
                                                        .multiMatch(multiMatchQry -> multiMatchQry
                                                                .fields(fields)
                                                                .fuzziness("auto")
                                                                .query(keywords)))
                                                .must(mustQry -> mustQry
                                                        .range(rangeQuery -> rangeQuery
                                                                .field(CREATED.getField())
                                                                .from(StringUtils.hasLength(year) ? year + "-01-01T00:00:00.000+08:00" : null)
                                                                .to(StringUtils.hasLength(year) ? year + "-12-31T23:59:59.999+08:00" : null)))
                                                .must(mustQry -> mustQry
                                                        .term(termQry -> termQry
                                                                .field(STATUS.getField()).value(StatusEnum.NORMAL.getCode())))))
                                .functions(function -> function
                                        .filter(filterQry -> filterQry
                                                .match(matchQry -> matchQry
                                                        .fuzziness("auto")
                                                        .field(TITLE.getField())
                                                        .query(keywords)))
                                        .weight(1.0))
                                .functions(function -> function
                                        .filter(filterQry -> filterQry
                                                .match(matchQry -> matchQry
                                                        .fuzziness("auto")
                                                        .field(DESCRIPTION.getField())
                                                        .query(keywords)))
                                        .weight(1.5))
                                .functions(function -> function
                                        .filter(filterQry -> filterQry
                                                .match(matchQry -> matchQry
                                                        .fuzziness("auto")
                                                        .field(CONTENT.getField())
                                                        .query(keywords)))
                                        .weight(2.0))
                                .scoreMode(FunctionScoreMode.Sum)
                                .boostMode(FunctionBoostMode.Multiply)))
                .withSort(sort -> sort
                        .score(score -> score
                                .order(SortOrder.Desc)))
                .withPageable(Objects.equals(currentPage, -1) ?
                        PageRequest.of(0, 10) :
                        PageRequest.of(currentPage - 1, blogPageSize))
                .withHighlightQuery(Boolean.TRUE.equals(allInfo) ?
                        ESHighlightBuilderUtils.blogHighlightQueryOrigin :
                        ESHighlightBuilderUtils.blogHighlightQuerySimple)
                .build();
        SearchHits<BlogDocument> search = elasticsearchTemplate.search(matchQuery, BlogDocument.class);

        return BlogDocumentVoConvertor.convert(search, blogPageSize, currentPage);
    }

    @Override
    public PageAdapter<BlogEntityVo> searchAllBlogs(String keywords, Integer currentPage, Integer size, Long userId) {

        var nativeQuery = NativeQuery.builder()
                .withQuery(query -> query
                        .functionScore(functionScore -> functionScore
                                .query(baseQry -> baseQry
                                        .bool(boolQry -> boolQry
                                                .must(mustQry -> mustQry
                                                        .term(termQry -> termQry
                                                                .field(USERID.getField())
                                                                .value(userId)))
                                                .must(mustQry -> mustQry
                                                        .multiMatch(multiMatchQry -> multiMatchQry
                                                                .fuzziness("auto")
                                                                .fields(fields)
                                                                .query(keywords)))))
                                .functions(function -> function
                                        .filter(filterQry -> filterQry
                                                .matchPhrase(matchPhraseQry -> matchPhraseQry
                                                        .field(TITLE.getField())
                                                        .query(keywords)))
                                        .weight(2.0))
                                .functions(function -> function
                                        .filter(filterQry -> filterQry
                                                .match(matchQry -> matchQry
                                                        .field(DESCRIPTION.getField())
                                                        .query(keywords)
                                                        .fuzziness("auto")))
                                        .weight(1.5))
                                .functions(function -> function
                                        .filter(filterQry -> filterQry
                                                .match(matchQry -> matchQry
                                                        .field(CONTENT.getField())
                                                        .query(keywords)
                                                        .fuzziness("auto")))
                                        .weight(1.0))
                                .scoreMode(FunctionScoreMode.Sum)
                                .boostMode(FunctionBoostMode.Multiply)))
                .withPageable(PageRequest.of(currentPage - 1, size))
                .withSort(sortQuery -> sortQuery
                        .score(score -> score
                                .order(SortOrder.Desc)))
                .build();

        SearchHits<BlogDocument> search = elasticsearchTemplate.search(nativeQuery, BlogDocument.class);
        return BlogEntityVoConvertor.convert(search, currentPage, size);
    }

}
