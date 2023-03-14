package com.chiu.megalith.coop.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;

import java.io.Serializable;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class SyncDto extends MessageDto implements Serializable {

    @EqualsAndHashCode(callSuper = true)
    @Data
    @SuperBuilder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Bind extends BaseBind implements Serializable {
        private String content;

    }
}
