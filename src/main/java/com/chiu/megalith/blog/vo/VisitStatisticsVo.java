package com.chiu.megalith.blog.vo;

import lombok.Builder;
import lombok.Data;

/**
 * @author mingchiuli
 * @create 2023-04-19 1:50 am
 */
@Data
@Builder
public class VisitStatisticsVo {

    private Long daySize;

    private Long weekSize;

    private Long monthSize;

    private Long yearSize;
}
