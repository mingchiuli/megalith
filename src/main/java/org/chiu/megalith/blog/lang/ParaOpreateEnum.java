package org.chiu.megalith.blog.lang;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum ParaOpreateEnum {

    NONE(0, "段内操作"),

    TAIL_APPEND(1, "从末尾增加一段"),

    TAIL_SUBTRACT(2, "从末尾减少一段");

    private final Integer code;

    private final String description;

    public static ParaOpreateEnum getInstance(Integer code) {
        for (ParaOpreateEnum value : ParaOpreateEnum.values()) {
            if (value.getCode().equals(code)) {
                return value;
            }
        }
        throw new IllegalArgumentException();
    }


}
