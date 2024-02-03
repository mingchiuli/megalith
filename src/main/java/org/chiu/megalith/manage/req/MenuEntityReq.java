package org.chiu.megalith.manage.req;

import org.chiu.megalith.infra.valid.ListValue;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

/**
 * @author mingchiuli
 * @create 2022-12-04 6:23 pm
 */
@Data
public class MenuEntityReq {

    private Long menuId;

    @NotNull
    private Long parentId;

    @NotBlank
    private String title;

    private String name;

    private String url;

    private String component;

    private String icon;

    @NotNull
    private Integer orderNum;

    @NotNull
    private Integer type;

    @ListValue(values = {0, 1})
    private Integer status;
}
