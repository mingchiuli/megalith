package com.chiu.megalith.manage.vo;

import com.chiu.megalith.common.valid.ListValue;
import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author mingchiuli
 * @create 2022-12-06 8:57 pm
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class RoleEntityVo {

    private Long id;

    @NotBlank
    private String name;

    @NotBlank
    private String code;

    private String remark;

    @ListValue(values = {0, 1})
    private Integer status;
}
