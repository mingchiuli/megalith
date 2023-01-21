package com.chiu.megalith.ws.dto.impl;

import com.chiu.megalith.ws.dto.Container;
import com.chiu.megalith.ws.dto.MessageDto;
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

    @Data
    @AllArgsConstructor
    @Builder
    @NoArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class Message implements Serializable {

        private Long from;

        private Long blogId;

        private String username;

        private String message;

        private List<Long> toAll;

        private Long toOne;
    }
}
