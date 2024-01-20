package org.chiu.megalith.manage.convertor;

import org.chiu.megalith.manage.entity.MenuEntity;
import org.chiu.megalith.manage.req.MenuEntityReq;

public class MenuEntityConvertor {

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
