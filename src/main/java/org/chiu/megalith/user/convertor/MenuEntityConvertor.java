package org.chiu.megalith.user.convertor;

import org.chiu.megalith.user.entity.MenuEntity;
import org.chiu.megalith.user.req.MenuEntityReq;

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
