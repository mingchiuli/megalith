package com.chiu.megalith.ws.dto.impl;

import com.chiu.megalith.ws.dto.BaseBind;
import com.chiu.megalith.ws.dto.Container;
import com.chiu.megalith.ws.dto.MessageDto;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.io.Serializable;

@Data
@AllArgsConstructor
public class SyncContentDto implements Serializable, MessageDto {

    private Container<Bind> content;

    @Override
    @SuppressWarnings("unchecked")
    public Container<Bind> getData() {
        return content;
    }

    @EqualsAndHashCode(callSuper = true)
    @Data
    @SuperBuilder
    @NoArgsConstructor
    public static class Bind extends BaseBind implements Serializable {
        private String content;

    }
}
