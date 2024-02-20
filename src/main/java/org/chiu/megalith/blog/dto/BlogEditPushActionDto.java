package org.chiu.megalith.blog.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.chiu.megalith.blog.lang.FieldEnum;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BlogEditPushActionDto {

    private String contentChange;

    private Integer version;

    private Integer indexStart;

    private Integer indexEnd;

    private FieldEnum fieldEnum;

    private Integer paraNo;

    private String redisKey;

    private String userKey;
}
