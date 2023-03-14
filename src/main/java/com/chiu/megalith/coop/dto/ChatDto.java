package com.chiu.megalith.coop.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.io.Serializable;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class ChatDto extends MessageDto implements Serializable {

    @EqualsAndHashCode(callSuper = true)
    @Data
    @SuperBuilder
    @AllArgsConstructor
    @NoArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class Bind extends BaseBind implements Serializable {

        private String username;

        private String message;

    }
}
