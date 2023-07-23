package org.chiu.megalith.coop.dto.impl;

import org.chiu.megalith.coop.dto.BaseDto;
import org.chiu.megalith.coop.vo.UserEntityVo;
import lombok.*;

@EqualsAndHashCode(callSuper = true)
@Data
public class JoinBlogDto extends BaseDto {

    private UserEntityVo user;
}
