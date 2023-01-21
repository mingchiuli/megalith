package com.chiu.megalith.websocket.dto.impl;

import com.chiu.megalith.websocket.dto.Container;
import com.chiu.megalith.websocket.dto.MessageDto;
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

    @EqualsAndHashCode(callSuper = true)
    @Data
    @AllArgsConstructor
    @Builder
    @NoArgsConstructor
    public static class Content extends BaseDto implements Serializable {
        private String content;

    }
}
