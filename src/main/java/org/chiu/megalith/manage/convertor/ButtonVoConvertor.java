package org.chiu.megalith.manage.convertor;

import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.manage.entity.MenuEntity;
import org.chiu.megalith.manage.vo.ButtonVo;

import java.util.List;
import java.util.stream.Stream;

/**
 * @Author limingjiu
 * @Date 2024/4/20 18:40
 **/
public class ButtonVoConvertor {

    public static List<ButtonVo> convert(List<MenuEntity> buttons, Boolean statusCheck) {
        Stream<MenuEntity> buttonStream = buttons.stream();
        if (Boolean.TRUE.equals(statusCheck)) {
            buttonStream = buttonStream.filter(menu -> StatusEnum.NORMAL.getCode().equals(menu.getStatus()));
        }

        return buttonStream
                .map(menu -> ButtonVo.builder()
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
