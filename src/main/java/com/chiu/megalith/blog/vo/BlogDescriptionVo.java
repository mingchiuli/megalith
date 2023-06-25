package com.chiu.megalith.blog.vo;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author mingchiuli
 * @create 2023-04-12 1:05 pm
 */
@Data
@Builder
public class BlogDescriptionVo {

    private Long id;

    private String title;

    private String description;

    private LocalDateTime created;

    private String link;
}
