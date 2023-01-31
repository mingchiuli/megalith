package com.chiu.megalith.search.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.ZonedDateTime;
import java.util.Collection;
import java.util.List;

/**
 * @author mingchiuli
 * @create 2021-12-12 6:55 AM
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BlogDocumentVo implements Serializable {

    private Long id;

    private Long userId;

    private Integer status;

    private String title;

    private String description;

    private String content;

    private String link;

    private ZonedDateTime created;

    private Float score;

    private Collection<List<String>> highlight;

}
