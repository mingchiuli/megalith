package com.chiu.megalith.manage.vo;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 6:23 pm
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MenuEntityVo implements Serializable {

    private Long menuId;

    private Long parentId;

    @NotBlank
    private String title;

    @NotBlank
    private String name;

    private String url;

    private String component;

    private Integer type;

    private String icon;

    @NotNull
    private Integer orderNum;

    @NotNull
    private Integer status;

    private List<MenuEntityVo> children = new ArrayList<>();
}
