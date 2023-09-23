package org.chiu.megalith.coop.dto.impl;

import org.chiu.megalith.coop.dto.BaseTransferDto;
import org.chiu.megalith.coop.dto.UserEntityDto;
import lombok.*;

@EqualsAndHashCode(callSuper = true)
@Data
public class JoinCoopDto extends BaseTransferDto {

    private UserEntityDto user;
}
