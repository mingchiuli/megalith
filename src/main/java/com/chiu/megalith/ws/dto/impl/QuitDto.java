package com.chiu.megalith.ws.dto.impl;

import com.chiu.megalith.ws.dto.BaseBind;
import com.chiu.megalith.ws.dto.Container;
import com.chiu.megalith.ws.dto.MessageDto;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

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
    @SuperBuilder
    @NoArgsConstructor
    public static class Bind extends BaseBind implements Serializable {}
}
