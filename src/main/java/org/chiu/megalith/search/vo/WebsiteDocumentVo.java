package org.chiu.megalith.search.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Builder;
import lombok.Data;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.Map;

/**
 * @author mingchiuli
 * @create 2022-12-23 8:06 pm
 */
@Data
@Builder
public class WebsiteDocumentVo {

    private String id;

    private Integer status;

    private String title;

    private String description;

    private String link;

    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private ZonedDateTime created;

    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private ZonedDateTime updated;

    private Float score;

    private Map<String, List<String>> highlight;

}
