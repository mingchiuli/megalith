package org.chiu.megalith.blog.vo;


import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class BlogDeleteVo {

    private Long id;

    private Long userId;

    private String title;

    private String description;

    private String content;

    private LocalDateTime created;

    private Integer status;

    private String link;

    private Long readCount;
}
