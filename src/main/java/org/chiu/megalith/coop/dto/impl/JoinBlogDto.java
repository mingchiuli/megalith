package org.chiu.megalith.coop.dto.impl;

import org.chiu.megalith.coop.dto.BaseDto;
import org.chiu.megalith.coop.vo.UserEntityVo;
import lombok.*;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
@AllArgsConstructor
public class JoinBlogDto extends BaseDto {

    private UserEntityVo user;
}
