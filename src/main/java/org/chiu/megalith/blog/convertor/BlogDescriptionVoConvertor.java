package org.chiu.megalith.blog.convertor;

import org.chiu.megalith.blog.dto.BlogDescriptionDto;
import org.chiu.megalith.blog.vo.BlogDescriptionVo;
import org.chiu.megalith.infra.page.PageAdapter;

import java.util.List;

public class BlogDescriptionVoConvertor {

    public static PageAdapter<BlogDescriptionVo> convert(PageAdapter<BlogDescriptionDto> page) {
        List<BlogDescriptionDto> dtos = page.getContent();
        List<BlogDescriptionVo> vos = dtos.stream().map(dto -> BlogDescriptionVo.builder()
                        .id(dto.getId())
                        .description(dto.getDescription())
                        .title(dto.getTitle())
                        .created(dto.getCreated())
                        .link(dto.getLink())
                        .build())
                .toList();

        return PageAdapter.<BlogDescriptionVo>builder()
                .content(vos)
                .first(page.isFirst())
                .last(page.isLast())
                .empty(page.isEmpty())
                .pageNumber(page.getPageNumber())
                .pageSize(page.getPageSize())
                .totalElements(page.getTotalElements())
                .totalPages(page.getTotalPages())
                .build();
    }
}
