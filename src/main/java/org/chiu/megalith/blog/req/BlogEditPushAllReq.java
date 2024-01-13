package org.chiu.megalith.blog.req;

import jakarta.validation.constraints.NotNull;
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
