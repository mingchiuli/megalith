package org.chiu.megalith.search.service.impl;

import co.elastic.clients.elasticsearch._types.SortOrder;
import org.chiu.megalith.blog.convertor.BlogEntityVoConvertor;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.infra.utils.ESHighlightBuilderUtils;
import org.chiu.megalith.infra.utils.JsonUtils;
import org.chiu.megalith.infra.utils.LuaScriptUtils;
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
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import static org.chiu.megalith.infra.lang.Const.HOT_READ;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * @author mingchiuli
 * @create 2022-11-30 9:00 pm
 */
@Service
@RequiredArgsConstructor
public class BlogSearchServiceImpl implements BlogSearchService {

    private final ElasticsearchTemplate elasticsearchTemplate;

    private final StringRedisTemplate redisTemplate;

    private final JsonUtils jsonUtils;

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
                                                        termQuery.field("status").value(StatusEnum.NORMAL.getCode())))
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

        return BlogDocumentVoConvertor.convert(search, blogPageSize, currentPage);
    }

    @Override
    @SuppressWarnings("unchecked")
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
        List<SearchHit<BlogDocument>> hits = search.getSearchHits();

        List<String> ids = hits.stream()
                .map(item -> String.valueOf(item.getContent().getId()))
                .toList();

        List<String> res = redisTemplate.execute(LuaScriptUtils.getHotBlogsLua, 
                Collections.singletonList(HOT_READ.getInfo()), 
                jsonUtils.writeValueAsString(ids));

        Map<Long, Integer> readMap = new HashMap<>();
        for (int i = 0; i < res.size(); i += 2) {
            readMap.put(Long.valueOf(res.get(i)), Integer.valueOf(res.get(i + 1)));
        }

        return BlogEntityVoConvertor.convert(search, readMap, currentPage, size);
    }

}
