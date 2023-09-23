package org.chiu.megalith.blog.vo;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author mingchiuli
 * @create 2022-12-03 11:36 pm
 */
@Builder
@Data
public class BlogEntityVo {

    private Long id;

    private String title;

    private String description;

    private String content;

    private Long readCount;

    private Double recentReadCount;

    private LocalDateTime created;

    private Integer status;
}
