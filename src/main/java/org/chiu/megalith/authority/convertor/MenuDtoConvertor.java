package org.chiu.megalith.authority.convertor;

import org.chiu.megalith.authority.dto.MenuDisplayDto;
import org.chiu.megalith.authority.dto.MenuDto;

import java.util.List;

public class MenuDtoConvertor {

    private MenuDtoConvertor() {}

    public static List<MenuDto> convert(List<MenuDisplayDto> menus) {
        return menus.stream()
                .map(menu -> MenuDto.builder()
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
