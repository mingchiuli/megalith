package com.chiu.megalith.websocket.dto.impl;

import com.chiu.megalith.websocket.dto.Container;
import com.chiu.megalith.websocket.dto.MessageDto;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

@Data
@AllArgsConstructor
@SuppressWarnings("unchecked")
public class ChatInfoDto implements Serializable, MessageDto {
    private Container<Message> message;

    @Override
    public Container<Message> getData() {
        return message;
    }

    @Data
    @AllArgsConstructor
    @Builder
    @NoArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class Message implements Serializable {
        private String message;
        private Long from;
        private List<Long> to;
        private Long blogId;
        private Long toOne;
    }
}
