package org.chiu.megalith.manage.dto;

import lombok.Builder;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * @Author limingjiu
 * @Date 2024/4/20 18:17
 **/
@Data
@Builder
public class MenusAndButtonsDto implements Serializable {

    private List<MenuDto> menus;

    private List<ButtonDto> buttons;

}
