package org.chiu.megalith.websocket.convertor;

import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.websocket.vo.BlogEditVo;

public class BlogEditVoConvertor {

    private BlogEditVoConvertor() {}

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
