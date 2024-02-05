package org.chiu.megalith.blog.convertor;

import org.chiu.megalith.blog.dto.BlogExhibitDto;
import org.chiu.megalith.blog.vo.BlogExhibitVo;

public class BlogExhibitVoConvertor {

    public static BlogExhibitVo convert(BlogExhibitDto dto) {
        return BlogExhibitVo.builder()
                .title(dto.getTitle())
                .description(dto.getDescription())
                .content(dto.getContent())
                .readCount(dto.getReadCount())
                .nickname(dto.getNickname())
                .avatar(dto.getAvatar())
                .created(dto.getCreated())
                .readCount(dto.getReadCount())
                .build();
    }
}
