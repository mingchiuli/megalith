package org.chiu.megalith.exhibit.convertor;

import org.chiu.megalith.exhibit.dto.BlogDescriptionDto;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.springframework.data.domain.Page;

/**
 * @Author limingjiu
 * @Date 2024/5/10 11:16
 **/
public class BlogDescriptionDtoConvertor {

    public static PageAdapter<BlogDescriptionDto> convert(Page<BlogEntity> page) {
        return new PageAdapter<>(page.map(blogEntity -> BlogDescriptionDto.builder()
                .id(blogEntity.getId())
                .description(blogEntity.getDescription())
                .title(blogEntity.getTitle())
                .created(blogEntity.getCreated())
                .link(blogEntity.getLink())
                .build()));
    }

}
