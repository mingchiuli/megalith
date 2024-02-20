package org.chiu.megalith.blog.convertor;

import org.chiu.megalith.blog.dto.BlogEditPushActionDto;
import org.chiu.megalith.blog.lang.FieldEnum;
import org.chiu.megalith.blog.req.BlogEditPushActionReq;

public class BlogEditPushActionDtoConvertor {

    public static BlogEditPushActionDto convert(String redisKey, String userKey, BlogEditPushActionReq req) {
        return BlogEditPushActionDto.builder()
                .contentChange(req.getContentChange())
                .indexEnd(req.getIndexEnd())
                .indexStart(req.getIndexStart())
                .paraNo(req.getParaNo())
                .version(req.getVersion())
                .fieldEnum(FieldEnum.getInstance(req.getField()))
                .redisKey(redisKey)
                .userKey(userKey)
                .build();
    }
}
