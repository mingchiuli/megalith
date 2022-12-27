package com.chiu.megalith.websocket.dto.impl;

import com.chiu.megalith.websocket.dto.Container;
import com.chiu.megalith.websocket.dto.MessageDto;
import lombok.AllArgsConstructor;
import lombok.Data;

import java.io.Serializable;

@Data
@AllArgsConstructor
public class TaskOverDto implements Serializable, MessageDto {

    private Container<String> from;


    @Override
    public Container<String> getData() {
        return from;
    }
}
