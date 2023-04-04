package com.chiu.megalith.coop.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * @author mingchiuli
 * @create 2023-04-03 11:36 pm
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BlogAbstractVo {

    private Long id;

    private String title;

    private Long readCount;

    private String description;

    private LocalDateTime created;
}
