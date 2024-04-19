package org.chiu.megalith.manage.convertor;

import org.chiu.megalith.manage.entity.BlogEntity;
import org.chiu.megalith.manage.vo.BlogEditVo;

public class BlogEditVoConvertor {

    public static BlogEditVo convert(BlogEntity blog, Integer version) {
        return BlogEditVo.builder()
                .userId(blog.getUserId())
                .id(blog.getId())
                .title(blog.getTitle())
                .description(blog.getDescription())
                .content(blog.getContent())
                .link(blog.getLink())
                .version(version)
                .status(blog.getStatus())
                .build();
    }
}
