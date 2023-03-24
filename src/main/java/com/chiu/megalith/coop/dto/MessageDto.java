package com.chiu.megalith.coop.dto;


import lombok.*;
import lombok.experimental.SuperBuilder;

import java.io.Serializable;


@SuperBuilder
@NoArgsConstructor
public abstract class MessageDto {

    protected Container<BaseBind> content;

    public Container<BaseBind> getData() {
        return content;
    }

    @Data
    @SuperBuilder
    @NoArgsConstructor
    public abstract static class BaseBind implements Serializable {
        protected Long fromId;

        protected Long blogId;

        protected Long toId;
    }

    @Data
    @AllArgsConstructor
    public static class Container<B> implements Serializable {
        B data;
    }

}

