package org.chiu.megalith.search.vo;

import lombok.Builder;
import lombok.Data;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

/**
 * @author mingchiuli
 * @create 2021-12-12 6:55 AM
 */
@Data
@Builder
public class BlogDocumentVo implements Serializable {

    private Long id;

    private Long userId;

    private Integer status;

    private String title;

    private String description;

    private String content;

    private String link;

    private LocalDateTime created;

    private Float score;

    private Map<String, List<String>> highlight;

}
