package org.chiu.megalith.exhibit.convertor;

import org.chiu.megalith.exhibit.dto.BlogExhibitDto;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.user.entity.UserEntity;

public class BlogExhibitDtoConvertor {

    private BlogExhibitDtoConvertor() {}

    public static BlogExhibitDto convert(BlogEntity blogEntity, UserEntity user) {
        return BlogExhibitDto.builder()
                .userId(blogEntity.getUserId())
                .title(blogEntity.getTitle())
                .description(blogEntity.getDescription())
                .content(blogEntity.getContent())
                .readCount(blogEntity.getReadCount())
                .nickname(user.getNickname())
                .avatar(user.getAvatar())
                .created(blogEntity.getCreated())
                .readCount(blogEntity.getReadCount())
                .build();
    }
}
