package com.chiu.megalith.ws.dto.impl;

import com.chiu.megalith.ws.dto.BaseBind;
import com.chiu.megalith.ws.dto.Container;
import com.chiu.megalith.ws.dto.MessageDto;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.io.Serializable;
import java.util.List;

@Data
@AllArgsConstructor
public class ChatDto implements Serializable, MessageDto {
    private Container<Bind> message;

    @Override
    @SuppressWarnings("unchecked")
    public Container<Bind> getData() {
        return message;
    }

    @EqualsAndHashCode(callSuper = true)
    @Data
    @SuperBuilder
    @NoArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class Bind extends BaseBind implements Serializable {

        private String username;

        private String message;

        private List<Long> toAll;

        private Long toOne;
    }
}
