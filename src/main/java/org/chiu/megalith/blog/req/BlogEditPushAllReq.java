package org.chiu.megalith.blog.req;

import jakarta.validation.constraints.NotBlank;
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

    @NotBlank
    private String title;

    @NotBlank
    private String description;
 
    @NotBlank
    private String content;

    @NotNull
    private Integer status;

    @NotBlank
    private String link;
}
