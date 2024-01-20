package org.chiu.megalith.search.convertor;

import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.search.document.WebsiteDocument;
import org.chiu.megalith.search.vo.WebsiteDocumentVo;
import org.springframework.data.elasticsearch.core.SearchHits;

import java.util.List;

public class WebsiteDocumentVoConvertor {

    public static PageAdapter<WebsiteDocumentVo> convert(SearchHits<WebsiteDocument> search, Integer pageSize, Integer currentPage) {

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

    public static WebsiteDocumentVo convert(WebsiteDocument document) {
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
