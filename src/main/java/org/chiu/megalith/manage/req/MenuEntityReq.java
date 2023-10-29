package org.chiu.megalith.manage.req;

import org.chiu.megalith.infra.valid.ListValue;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


/**
 * @author mingchiuli
 * @create 2022-12-04 6:23 pm
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
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

    @NotNull
    private Integer type;

    @NotBlank
    private String icon;

    private Integer orderNum;

    @ListValue(values = {0, 1})
    private Integer status;
}
