package org.chiu.megalith.manage.vo;

import lombok.Builder;
import lombok.Data;


@Data
@Builder
public class BlogEditVo {

    private Long id;

    private Long userId;

    private String title;

    private String description;

    private String link;

    private String content;

    private Integer status;

    private Integer version;
}
