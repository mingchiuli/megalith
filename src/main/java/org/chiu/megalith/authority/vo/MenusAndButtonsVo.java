package org.chiu.megalith.authority.vo;

import lombok.Builder;
import lombok.Data;

import java.util.List;

/**
 * @Author limingjiu
 * @Date 2024/4/20 18:17
 **/
@Data
@Builder
public class MenusAndButtonsVo {

    private List<MenuVo> menus;

    private List<ButtonVo> buttons;

}
