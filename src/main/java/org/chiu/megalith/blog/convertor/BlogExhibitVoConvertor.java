package org.chiu.megalith.blog.convertor;

import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.vo.BlogExhibitVo;
import org.chiu.megalith.manage.entity.UserEntity;

public class BlogExhibitVoConvertor {

    public static BlogExhibitVo convert(BlogEntity blogEntity, UserEntity user) {
        return BlogExhibitVo.builder()
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
