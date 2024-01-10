package org.chiu.megalith.blog.lang;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum PushActionEnum {

    NONE(-1, "Status操作"),

    TAIL_APPEND(0, "从末尾向后添加"),

    TAIL_SUBTRACT(1, "从末尾向前减少"),

    HEAD_APPEND(2, "从开头增加"),

    HEAD_SUBTRACT(3, "从开头减少"),

    REPLACE(4, "替换文本"),

    REMOVE(5, "删除所有");

    private final Integer code;

    private final String description;

    public static PushActionEnum getInstance(Integer code) {
        for (PushActionEnum value : PushActionEnum.values()) {
            if (value.getCode().equals(code)) {
                return value;
            }
        }
        throw new IllegalArgumentException();
    }
}
