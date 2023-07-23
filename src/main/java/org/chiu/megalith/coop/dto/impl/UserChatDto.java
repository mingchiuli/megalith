package org.chiu.megalith.coop.dto.impl;

import org.chiu.megalith.coop.dto.BaseDto;
import lombok.*;


@EqualsAndHashCode(callSuper = true)
@Data
public class UserChatDto extends BaseDto {

    private String username;

    private String message;
}
