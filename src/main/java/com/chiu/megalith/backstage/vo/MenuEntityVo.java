package com.chiu.megalith.backstage.vo;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 6:23 pm
 */
@Data
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
