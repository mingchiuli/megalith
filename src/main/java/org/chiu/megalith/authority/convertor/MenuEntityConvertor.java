package org.chiu.megalith.authority.convertor;

import org.chiu.megalith.authority.entity.MenuEntity;
import org.chiu.megalith.authority.req.MenuEntityReq;

public class MenuEntityConvertor {

    private MenuEntityConvertor() {}

    public static MenuEntity convert(MenuEntityReq menu) {
        return MenuEntity.builder()
                .menuId(menu.getMenuId())
                .parentId(menu.getParentId())
                .icon(menu.getIcon())
                .url(menu.getUrl())
                .title(menu.getTitle())
                .name(menu.getName())
                .component(menu.getComponent())
                .type(menu.getType())
                .orderNum(menu.getOrderNum())
                .status(menu.getStatus())
                .build();
    }
}
