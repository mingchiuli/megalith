package org.chiu.megalith.search.convertor;

import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.search.document.BlogDocument;
import org.chiu.megalith.search.vo.BlogDocumentVo;
import org.springframework.data.elasticsearch.core.SearchHits;

import java.util.List;

public class BlogDocumentVoConvertor {

    public static PageAdapter<BlogDocumentVo> convert(SearchHits<BlogDocument> search, Integer blogPageSize, Integer currentPage) {
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
                .pageSize(blogPageSize)
                .pageNumber(currentPage)
                .empty(totalHits == 0)
                .totalElements(totalHits)
                .totalPages((int) totalPage)
                .content(vos)
                .build();
    }
}
