package com.chiu.megalith.websocket.dto.impl;

import com.chiu.megalith.websocket.dto.Container;
import com.chiu.megalith.websocket.dto.MessageDto;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;

import java.io.Serializable;
import java.util.List;

@Data
@AllArgsConstructor
public class ChatInfoDto implements Serializable, MessageDto {
    private Container<Message> message;

    @Override
    @SuppressWarnings("unchecked")
    public Container<Message> getData() {
        return message;
    }

    @EqualsAndHashCode(callSuper = true)
    @Data
    @AllArgsConstructor
    @Builder
    @NoArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class Message extends BaseDto implements Serializable {

        private String username;

        private String message;

        private List<Long> toAll;

        private Long toOne;
    }
}
