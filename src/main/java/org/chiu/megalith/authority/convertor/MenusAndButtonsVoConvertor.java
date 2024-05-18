package org.chiu.megalith.authority.convertor;

import org.chiu.megalith.authority.dto.ButtonDto;
import org.chiu.megalith.authority.dto.MenuDto;
import org.chiu.megalith.authority.dto.MenusAndButtonsDto;
import org.chiu.megalith.authority.vo.ButtonVo;
import org.chiu.megalith.authority.vo.MenuVo;
import org.chiu.megalith.authority.vo.MenusAndButtonsVo;

import java.util.ArrayList;
import java.util.List;

/**
 * @Author limingjiu
 * @Date 2024/5/10 12:51
 **/
public class MenusAndButtonsVoConvertor {

    private MenusAndButtonsVoConvertor() {}

    public static MenusAndButtonsVo convertor(MenusAndButtonsDto dto) {
        List<ButtonDto> buttons = dto.getButtons();
        List<MenuDto> menus = dto.getMenus();

        List<ButtonVo> buttonVos = new ArrayList<>();
        List<MenuVo> menuVos = new ArrayList<>();

        buttons.forEach(button -> buttonVos.add(ButtonVo.builder()
                .menuId(button.getMenuId())
                .parentId(button.getParentId())
                .icon(button.getIcon())
                .url(button.getUrl())
                .title(button.getTitle())
                .name(button.getName())
                .component(button.getComponent())
                .type(button.getType())
                .orderNum(button.getOrderNum())
                .status(button.getStatus())
                .build()
        ));

        menus.forEach(menu -> menuVos.add(MenuVo.builder()
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
                .build()));

        return MenusAndButtonsVo.builder()
                .buttons(buttonVos)
                .menus(menuVos)
                .build();

    }

    private static List<MenuVo> convert(List<MenuDto> menuVos) {
        return menuVos.stream()
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
