package org.chiu.megalith.blog.req;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BlogEditPushAllReq {

    private Long id;

    private String title;

    private String description;

    private String content;

    private Integer status;

    private String link;
}
