package org.chiu.megalith.user.convertor;

import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.user.dto.ButtonDto;
import org.chiu.megalith.user.entity.MenuEntity;

import java.util.List;
import java.util.stream.Stream;

/**
 * @Author limingjiu
 * @Date 2024/4/20 18:40
 **/
public class ButtonDtoConvertor {

    private ButtonDtoConvertor() {}


    public static List<ButtonDto> convert(List<MenuEntity> buttons, Boolean statusCheck) {
        Stream<MenuEntity> buttonStream = buttons.stream();
        if (Boolean.TRUE.equals(statusCheck)) {
            buttonStream = buttonStream.filter(menu -> StatusEnum.NORMAL.getCode().equals(menu.getStatus()));
        }

        return buttonStream
                .map(menu -> ButtonDto.builder()
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
}
