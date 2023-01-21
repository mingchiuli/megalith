package com.chiu.megalith.websocket.dto.impl;

import com.chiu.megalith.websocket.dto.Container;
import com.chiu.megalith.websocket.dto.MessageDto;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;

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


    @EqualsAndHashCode(callSuper = true)
    @Data
    @AllArgsConstructor
    public static class Bind extends BaseDto implements Serializable {}
}
