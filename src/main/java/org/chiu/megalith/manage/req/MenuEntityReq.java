package org.chiu.megalith.manage.req;

import org.chiu.megalith.infra.valid.ListValue;
import jakarta.validation.constraints.NotBlank;
import lombok.Builder;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 6:23 pm
 */
@Data
@Builder
public class MenuEntityReq {

    private Long menuId;

    private Long parentId;

    @NotBlank
    private String title;

    @NotBlank
    private String name;

    @NotBlank
    private String url;

    @NotBlank
    private String component;

    @NotBlank
    private Integer type;

    @NotBlank
    private String icon;

    private Integer orderNum;

    @ListValue(values = {0, 1})
    private Integer status;

    @Builder.Default
    private List<MenuEntityReq> children = new ArrayList<>();
}
