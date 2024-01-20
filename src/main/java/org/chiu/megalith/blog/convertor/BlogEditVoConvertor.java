package org.chiu.megalith.blog.convertor;

import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.vo.BlogEditVo;

public class BlogEditVoConvertor {

    public static BlogEditVo convert(BlogEntity blog) {
        return BlogEditVo.builder()
                .id(blog.getId())
                .title(blog.getTitle())
                .description(blog.getDescription())
                .content(blog.getContent())
                .link(blog.getLink())
                .status(blog.getStatus())
                .build();
    }
}
