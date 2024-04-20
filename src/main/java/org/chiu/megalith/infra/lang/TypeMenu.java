package org.chiu.megalith.infra.lang;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @Author limingjiu
 * @Date 2024/4/20 18:31
 **/
@Getter
@AllArgsConstructor
public enum TypeMenu {

    CATALOGUE(0, "分类"),

    MENU(1, "菜单"),

    BUTTON(2, "按钮");

    private final Integer code;

    private final String desc;
}
