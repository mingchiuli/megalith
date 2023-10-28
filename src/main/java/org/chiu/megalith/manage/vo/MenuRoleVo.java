package org.chiu.megalith.manage.vo;

import lombok.Builder;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
public class MenuRoleVo {

    private Long menuId;

    private String title;

    //是否选了
    private Boolean check;

    @Builder.Default
    private List<MenuRoleVo> children = new ArrayList<>();
}
