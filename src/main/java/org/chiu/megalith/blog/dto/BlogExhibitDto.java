package org.chiu.megalith.blog.dto;

import lombok.*;

import java.time.LocalDateTime;

@Data
@AllArgsConstructor
@Builder
@NoArgsConstructor
public class BlogExhibitDto {

    private Long userId;

    private String description;

    private String nickname;

    private String avatar;

    private String title;

    private String content;

    private LocalDateTime created;

    private Long readCount;

}
