package com.chiu.megalith.exhibit.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author mingchiuli
 * @create 2023-03-30 3:15 am
 */
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class BlogHotReadVo {

    private Long id;

    private String title;

    private Long readCount;
}
