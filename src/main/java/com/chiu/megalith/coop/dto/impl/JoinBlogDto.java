package com.chiu.megalith.coop.dto.impl;

import com.chiu.megalith.coop.dto.BaseDto;
import com.chiu.megalith.coop.vo.UserEntityVo;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.io.Serializable;
@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
@AllArgsConstructor
public class JoinBlogDto extends BaseDto implements Serializable {

    private UserEntityVo user;
}
