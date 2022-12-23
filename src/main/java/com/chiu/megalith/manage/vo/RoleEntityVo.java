package com.chiu.megalith.manage.vo;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
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

    @NotNull
    private Integer status;
}
