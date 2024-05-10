package org.chiu.megalith.authority.convertor;

import org.chiu.megalith.authority.vo.MenuDisplayVo;
import org.chiu.megalith.authority.vo.MenuVo;

import java.util.List;

public class MenuVoConvertor {

    public static List<MenuVo> convert(List<MenuDisplayVo> menus) {
        return menus.stream()
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
                        .children(convert(menu.getChildren()))
                        .build())
                .toList();
    }
}
