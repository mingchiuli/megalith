package com.chiu.megalith.coop.dto.impl;

import com.chiu.megalith.coop.dto.MessageDto;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.io.Serializable;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class QuitBlogDto extends MessageDto implements Serializable {
    @EqualsAndHashCode(callSuper = true)
    @Data
    @SuperBuilder
    @AllArgsConstructor
    @SuppressWarnings("unused")
    public static class Bind extends BaseBind implements Serializable {}
}
