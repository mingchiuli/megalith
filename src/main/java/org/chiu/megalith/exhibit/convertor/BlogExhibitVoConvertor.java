package org.chiu.megalith.exhibit.convertor;

import org.chiu.megalith.exhibit.dto.BlogExhibitDto;
import org.chiu.megalith.exhibit.vo.BlogExhibitVo;

public class BlogExhibitVoConvertor {

    private BlogExhibitVoConvertor() {}

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
