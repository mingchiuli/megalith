package com.chiu.megalith.coop.dto.impl;

import com.chiu.megalith.coop.dto.BaseDto;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.io.Serializable;

@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
@SuperBuilder
public class ChatUserDto extends BaseDto implements Serializable {
    private String username;

    private String message;
}
