package org.chiu.megalith.manage.req;

import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Data
public class BlogEditPushAllReq {

    private Long id;

    @NotNull
    private Long userId;

    @NotNull
    private String title;

    @NotNull
    private String description;

    @NotNull
    private String content;

    @NotNull
    private Integer status;

    @NotNull
    private String link;
}
