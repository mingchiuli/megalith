package org.chiu.megalith.infra.lang;

import lombok.Getter;

@Getter
public enum StatusEnum {

    NORMAL(0, "正常状态"),

    HIDE(1, "隐藏/禁用状态");

    private final Integer code;

    private final String description;

    StatusEnum(Integer code, String description) {
        this.code = code;
        this.description = description;
    }
}
