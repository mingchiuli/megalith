package org.chiu.megalith.manage.convertor;

import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.manage.entity.MenuEntity;
import org.chiu.megalith.manage.vo.MenuVo;

import java.util.List;
import java.util.stream.Stream;

public class MenuVoConvertor {

    public static List<MenuVo> convert(List<MenuEntity> menus, boolean statusCheck) {
        Stream<MenuEntity> menuStream = menus.stream();
        if (Boolean.TRUE.equals(statusCheck)) {
            menuStream = menuStream.filter(menu -> StatusEnum.NORMAL.getCode().equals(menu.getStatus()));
        }

        return menuStream
                .map(menu -> MenuVo.builder()
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
                        .build())
                .toList();
    }

    public static List<MenuVo> convert(List<MenuEntity> menus) {
        return menus.stream()
                .map(menu -> MenuVo.builder()
                        .menuId(menu.getMenuId())
                        .parentId(menu.getParentId())
                        .icon(menu.getIcon())
                        .url(menu.getUrl())
                        .title(menu.getTitle())
                        .name(menu.getName())
                        .component(menu.getComponent())
                        .name(menu.getName())
                        .type(menu.getType())
                        .orderNum(menu.getOrderNum())
                        .status(menu.getStatus())
                        .build())
                .toList();
    }
}
