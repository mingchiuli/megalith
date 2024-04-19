package org.chiu.megalith.blog.convertor;

import org.chiu.megalith.manage.entity.BlogEntity;
import org.chiu.megalith.blog.vo.BlogDescriptionVo;
import org.chiu.megalith.infra.page.PageAdapter;
import org.springframework.data.domain.Page;

public class BlogDescriptionVoConvertor {

    public static PageAdapter<BlogDescriptionVo> convert(Page<BlogEntity> page) {
        return new PageAdapter<>(page.map(blogEntity -> BlogDescriptionVo.builder()
                .id(blogEntity.getId())
                .description(blogEntity.getDescription())
                .title(blogEntity.getTitle())
                .created(blogEntity.getCreated())
                .link(blogEntity.getLink())
                .build()));
    }
}
