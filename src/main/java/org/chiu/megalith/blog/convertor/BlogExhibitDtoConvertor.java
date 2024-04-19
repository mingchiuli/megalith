package org.chiu.megalith.blog.convertor;

import org.chiu.megalith.blog.dto.BlogExhibitDto;
import org.chiu.megalith.manage.entity.BlogEntity;
import org.chiu.megalith.manage.entity.UserEntity;

public class BlogExhibitDtoConvertor {

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
