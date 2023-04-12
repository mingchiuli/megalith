package com.chiu.megalith.exhibit.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * @author mingchiuli
 * @create 2023-04-12 1:05 pm
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BlogDescriptionVo {

    private Long id;

    private String title;

    private String description;

    private LocalDateTime created;

    private String link;
}
