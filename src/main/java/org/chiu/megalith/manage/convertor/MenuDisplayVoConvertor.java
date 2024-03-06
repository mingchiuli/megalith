package org.chiu.megalith.manage.convertor;

import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.manage.entity.MenuEntity;
import org.chiu.megalith.manage.vo.MenuDisplayVo;

import java.util.List;
import java.util.stream.Stream;

public class MenuDisplayVoConvertor {

    public static List<MenuDisplayVo> convert(List<MenuEntity> menus, boolean statusCheck) {
        Stream<MenuEntity> menuStream = menus.stream();
        if (Boolean.TRUE.equals(statusCheck)) {
            menuStream = menuStream.filter(menu -> StatusEnum.NORMAL.getCode().equals(menu.getStatus()));
        }

        return menuStream
                .map(menu -> MenuDisplayVo.builder()
                        .menuId(menu.getMenuId())
                        .parentId(menu.getParentId())
                        .icon(menu.getIcon())
                        .url(menu.getUrl())
                        .updated(menu.getUpdated())
                        .created(menu.getCreated())
                        .title(menu.getTitle())
                        .name(menu.getName())
                        .component(menu.getComponent())
                        .type(menu.getType())
                        .orderNum(menu.getOrderNum())
                        .status(menu.getStatus())
                        .build())
                .toList();
    }
}
