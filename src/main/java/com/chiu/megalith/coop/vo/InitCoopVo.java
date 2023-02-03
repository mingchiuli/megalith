package com.chiu.megalith.coop.vo;

import com.chiu.megalith.exhibit.entity.BlogEntity;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

/**
 * @author mingchiuli
 * @create 2023-01-14 1:55 am
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class InitCoopVo implements Serializable {

    private BlogEntity blogEntity;

    private List<String> userEntityVos;
}
