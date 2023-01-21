package com.chiu.megalith.ws.dto.impl;

import com.chiu.megalith.ws.dto.Container;
import com.chiu.megalith.ws.dto.MessageDto;
import lombok.*;

import java.io.Serializable;

@Data
@AllArgsConstructor
@Builder
@NoArgsConstructor
public class DestroyDto implements Serializable, MessageDto {
    private Container<Bind> data;

    @Override
    @SuppressWarnings("unchecked")
    public Container<Bind> getData() {
        return data;
    }

    @Data
    @AllArgsConstructor
    @Builder
    @NoArgsConstructor
    public static class Bind implements Serializable {
        private Long from;

        private Long blogId;
    }
}
