package com.chiu.megalith.blog.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author mingchiuli
 * @create 2023-04-19 1:50 am
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class VisitStatisticsVo {

    public Long daySize;

    public Long weekSize;

    public Long monthSize;

    public Long yearSize;
}
