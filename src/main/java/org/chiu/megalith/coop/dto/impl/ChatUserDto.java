package org.chiu.megalith.coop.dto.impl;

import org.chiu.megalith.coop.dto.BaseDto;
import lombok.*;
import lombok.experimental.SuperBuilder;


@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
@SuperBuilder
public class ChatUserDto extends BaseDto {
    private String username;

    private String message;
}
