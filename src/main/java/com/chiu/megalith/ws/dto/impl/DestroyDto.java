package com.chiu.megalith.ws.dto.impl;

import com.chiu.megalith.ws.dto.BaseBind;
import com.chiu.megalith.ws.dto.Container;
import com.chiu.megalith.ws.dto.MessageDto;
import lombok.*;
import lombok.experimental.SuperBuilder;

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

    @EqualsAndHashCode(callSuper = true)
    @Data
    @SuperBuilder
    public static class Bind extends BaseBind implements Serializable {}
}
