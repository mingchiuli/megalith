package org.chiu.megalith.manage.lang;

import lombok.Getter;

@Getter
public enum MessageActionFieldEnum {

    TITLE("title"),

    DESCRIPTION("description"),

    USER_ID("userId"),

    CONTENT("content"),

    LINK("link"),

    STATUS("status"),

    ID("id"),

    VERSION("version");
    
    private final String msg;

    MessageActionFieldEnum(String msg) {
        this.msg = msg;
    }
}
