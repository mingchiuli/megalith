package org.chiu.megalith.manage.vo;

import lombok.Builder;
import lombok.Data;

import java.util.List;

/**
 * @Author limingjiu
 * @Date 2024/4/20 18:17
 **/
@Data
@Builder
public class MenusAndButtons {

    private List<MenuVo> menus;

    private List<ButtonVo> buttons;

}
