package com.chiu.megalith.exhibit.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * @author mingchiuli
 * @create 2022-12-03 11:36 pm
 */
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
public class BlogEntityDto implements Serializable {

    private Long id;

    private String title;

    private String description;

    private String content;

    private LocalDateTime created;

    private Integer status;

    private Integer readRecent;
}
