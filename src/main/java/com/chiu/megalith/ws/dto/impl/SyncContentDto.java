package com.chiu.megalith.ws.dto.impl;

import com.chiu.megalith.ws.dto.Container;
import com.chiu.megalith.ws.dto.MessageDto;
import lombok.*;

import java.io.Serializable;

@Data
@AllArgsConstructor
public class SyncContentDto implements Serializable, MessageDto {

    private Container<Content> content;

    @Override
    @SuppressWarnings("unchecked")
    public Container<Content> getData() {
        return content;
    }

    @Data
    @AllArgsConstructor
    @Builder
    @NoArgsConstructor
    public static class Content implements Serializable {
        private Long from;

        private Long blogId;
        private String content;

    }
}
