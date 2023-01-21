package com.chiu.megalith.ws.dto.impl;

import com.chiu.megalith.ws.dto.Container;
import com.chiu.megalith.ws.dto.MessageDto;
import lombok.AllArgsConstructor;
import lombok.Data;

import java.io.Serializable;

@Data
@AllArgsConstructor
public class QuitDto implements MessageDto, Serializable {

    private Container<QuitDto.Bind> from;


    @Override
    @SuppressWarnings("unchecked")
    public Container<QuitDto.Bind> getData() {
        return from;
    }


    @Data
    @AllArgsConstructor
    public static class Bind implements Serializable {
        private Long from;

        private Long blogId;
    }
}
