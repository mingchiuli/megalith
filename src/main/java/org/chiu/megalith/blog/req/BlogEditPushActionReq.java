package org.chiu.megalith.blog.req;

import lombok.Data;

@Data
public class BlogEditPushActionReq {

    private Long id;

    private String title;

    private String description;

    //变动的部分
    private String contentChange;

    private Integer operateTypeCode;

    private Integer version;

    private Integer status;

    private String link;
}
