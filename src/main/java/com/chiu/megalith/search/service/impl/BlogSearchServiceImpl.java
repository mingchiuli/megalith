package com.chiu.megalith.search.service.impl;

import co.elastic.clients.elasticsearch._types.SortOrder;
import com.chiu.megalith.authentication.user.entity.UserEntity;
import com.chiu.megalith.authentication.user.service.UserService;
import com.chiu.megalith.blog.dto.BlogEntityDto;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.page.PageAdapter;
import com.chiu.megalith.search.document.BlogDocument;
import com.chiu.megalith.search.service.BlogSearchService;
import com.chiu.megalith.search.vo.BlogDocumentVo;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.client.elc.NativeQuery;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.query.HighlightQuery;
import org.springframework.data.elasticsearch.core.query.highlight.Highlight;
import org.springframework.data.elasticsearch.core.query.highlight.HighlightField;
import org.springframework.data.elasticsearch.core.query.highlight.HighlightParameters;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
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

    ElasticsearchTemplate elasticsearchTemplate;

    StringRedisTemplate redisTemplate;

    UserService userService;

    private final int blogPageSize = Integer.parseInt(Const.BLOG_PAGE_SIZE.getMsg());

    @Override
    public PageAdapter<BlogDocumentVo> selectBlogsByES(Integer currentPage, String keyword, Integer flag, Integer year) {

        HighlightParameters highlightParameters;
        HighlightParameters.HighlightParametersBuilder highlightParametersBuilder = new HighlightParameters.
                HighlightParametersBuilder().
                withPreTags("<b style='color:red'>").
                withPostTags("</b>");
        if (flag == 0) {
            highlightParametersBuilder
                    .withNumberOfFragments(1)
                    .withFragmentSize(5);
        }

        highlightParameters = highlightParametersBuilder.build();

        List<HighlightField> fields = Arrays.asList(
                new HighlightField("title"),
                new HighlightField("description"),
                new HighlightField("content"));

        Highlight highlight = new Highlight(highlightParameters, fields);

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
                                                from(year == null ? null : year + "-01-01T00:00:00.000").
                                                to(year == null ? null : year + "-12-31T23:59:59.999")))))
                .withSort(sort -> sort.
                        score(score -> score.
                                order(SortOrder.Desc)))
                .withPageable(PageRequest.of(currentPage - 1, blogPageSize))
                .withHighlightQuery(new HighlightQuery(highlight, null))
                .build();

        SearchHits<BlogDocument> search = elasticsearchTemplate.search(matchQuery, BlogDocument.class);

        ArrayList<BlogDocumentVo> vos = new ArrayList<>();

        search.getSearchHits().forEach(hit -> {
            BlogDocumentVo vo = new BlogDocumentVo();
            BeanUtils.copyProperties(hit.getContent(), vo);
            vo.setScore(hit.getScore());
            vo.setHighlight(hit.getHighlightFields().values().toString());
            vos.add(vo);
        });

        return PageAdapter.
                <BlogDocumentVo>builder().
                first(currentPage == 1).
                last(currentPage == (search.getTotalHits() % blogPageSize == 0 ? search.getTotalHits() / blogPageSize : search.getTotalHits() / blogPageSize + 1)).
                pageSize(blogPageSize).
                pageNumber(currentPage).
                empty(search.isEmpty()).
                totalElements(search.getTotalHits()).
                totalPages((int) (search.getTotalHits() % blogPageSize == 0 ? search.getTotalHits() / blogPageSize : (search.getTotalHits() / blogPageSize + 1))).
                content(vos).
                build();
    }

    @Override
    public PageAdapter<BlogEntityDto> searchAllBlogs(String keyword, Integer currentPage, Integer size) {
        NativeQuery nativeQuery = NativeQuery.builder()
                .withQuery(query ->
                        query.multiMatch(multiQuery -> multiQuery.
                                fields(Arrays.asList("title", "description", "content")).query(keyword)))
                .withPageable(PageRequest.of(currentPage - 1, size))
                .withSort(sortQuery -> sortQuery.
                        field(fieldQuery -> fieldQuery.
                                field("created").order(SortOrder.Desc))).build();

        SearchHits<BlogDocument> search = elasticsearchTemplate.search(nativeQuery, BlogDocument.class);

        List<BlogEntityDto> entities = new ArrayList<>();
        search.getSearchHits().forEach(hit -> {
            BlogDocument content = hit.getContent();
            BlogEntityDto entityDto = new BlogEntityDto();
            BeanUtils.copyProperties(content, entityDto);
            Integer readNum = Integer.valueOf(Optional.ofNullable(redisTemplate.opsForValue().get(Const.READ_RECENT.getMsg() + content.getId())).orElse("0"));
            UserEntity userEntity = userService.findUsernameById(content.getUserId());
            entityDto.setUsername(userEntity.getUsername());
            entityDto.setReadRecent(readNum);
            entities.add(entityDto);
        });

        return PageAdapter.<BlogEntityDto>builder().
                totalPages((int) (search.getTotalHits() % size == 0 ? search.getTotalHits() / size : (search.getTotalHits() / size + 1))).
                totalElements(search.getTotalHits()).
                pageNumber(currentPage).
                pageSize(size).
                empty(search.isEmpty()).
                first(currentPage == 1).
                last(currentPage == (search.getTotalHits() % size == 0 ? search.getTotalHits() / size : search.getTotalHits() / size + 1)).
                content(entities).
                build();
    }

}
