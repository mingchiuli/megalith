package com.chiu.megalith.websocket.dto.impl;

import com.chiu.megalith.websocket.dto.Container;
import com.chiu.megalith.websocket.dto.MessageDto;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;

@Data
@AllArgsConstructor
public class DestroyDto implements Serializable, MessageDto {
    private Container<Bind> data;

    @Override
    @SuppressWarnings("unchecked")
    public Container<Bind> getData() {
        return data;
    }

    @EqualsAndHashCode(callSuper = true)
    @Data
    @AllArgsConstructor
    public static class Bind extends BaseDto implements Serializable {}
}
