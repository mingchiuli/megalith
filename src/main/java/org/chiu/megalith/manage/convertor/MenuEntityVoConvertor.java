package org.chiu.megalith.manage.convertor;

import org.chiu.megalith.manage.entity.MenuEntity;
import org.chiu.megalith.manage.vo.MenuEntityVo;

public class MenuEntityVoConvertor {

    public static MenuEntityVo convert(MenuEntity menuEntity) {
        return MenuEntityVo.builder()
                .menuId(menuEntity.getMenuId())
                .url(menuEntity.getUrl())
                .title(menuEntity.getTitle())
                .type(menuEntity.getType())
                .name(menuEntity.getName())
                .component(menuEntity.getComponent())
                .orderNum(menuEntity.getOrderNum())
                .parentId(menuEntity.getParentId())
                .icon(menuEntity.getIcon())
                .status(menuEntity.getStatus())
                .build();
    }
}
