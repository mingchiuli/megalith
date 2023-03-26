package com.chiu.megalith.exhibit.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * @author mingchiuli
 * @create 2023-03-19 3:27 am
 */
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class BlogExhibitVo {
    private String nickname;

    private String avatar;

    private String title;

    private String content;

    private LocalDateTime created;

    private Long readCount;
}
