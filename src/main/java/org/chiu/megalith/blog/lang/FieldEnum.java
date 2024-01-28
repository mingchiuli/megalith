package org.chiu.megalith.blog.lang;

import lombok.Getter;

@Getter

public enum FieldEnum {

    DESCRIPTION("description", "描述"),

    LINK("link", "封面"),

    TITLE("title", "标题"),

    STATUS("status", "文章状态"),

    CONTENT("content", "内容"),

    CREATED("created", "创建时间"),

    USERID("userId", "用户ID");

    private final String field;

    private final String description;

    FieldEnum(String field, String description) {
        this.field = field;
        this.description = description;
    }

    public static FieldEnum getInstance(String field) {
        for (FieldEnum value : FieldEnum.values()) {
            if (value.getField().equals(field)) {
                return value;
            }
        }
        throw new IllegalArgumentException();
    }
}
