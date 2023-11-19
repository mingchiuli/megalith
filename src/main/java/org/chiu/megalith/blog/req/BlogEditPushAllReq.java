package org.chiu.megalith.blog.req;

import lombok.Data;

@Data
public class BlogEditPushAllReq {
    private Long id;

    private String title;

    private String description;

    private String content;

    private Integer status;

    private String link;
}
