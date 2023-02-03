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
 * @create 2022-12-23 8:06 pm
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class WebsiteDocumentVo implements Serializable {

    private String id;

    private Integer status;

    private String title;

    private String description;

    private String link;

    private ZonedDateTime created;

    private Float score;

    private Collection<List<String>> highlight;

}
