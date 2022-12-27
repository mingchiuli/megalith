package com.chiu.megalith.websocket.dto.impl;

import com.chiu.megalith.websocket.dto.Container;
import com.chiu.megalith.websocket.dto.MessageDto;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
public class SyncContentDto implements Serializable, MessageDto {

    private Container<Content> content;

    @Override
    public Container<Content> getData() {
        return content;
    }

    @Data
    @AllArgsConstructor
    @Builder
    @NoArgsConstructor
    public static class Content implements Serializable {
        private Long from;
        private String content;
        private Long blogId;
    }
}
