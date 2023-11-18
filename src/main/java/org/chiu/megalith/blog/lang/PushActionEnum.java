package org.chiu.megalith.blog.lang;

import lombok.Getter;

@Getter
public enum PushActionEnum {
    APPEND(0, "向后添加"),

    SUBSTRACT(1, "向前减少"),
    
    OTHER(2, "其他操作");

    private final Integer code;

    private final String description;

    PushActionEnum(Integer code, String description) {
        this.code = code;
        this.description = description;
    }
}
