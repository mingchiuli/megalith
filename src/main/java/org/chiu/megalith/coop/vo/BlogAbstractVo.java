package org.chiu.megalith.coop.vo;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author mingchiuli
 * @create 2023-04-03 11:36 pm
 */
@Data
@Builder
public class BlogAbstractVo {

    private Long id;

    private String title;

    private String description;

    private LocalDateTime created;
}
