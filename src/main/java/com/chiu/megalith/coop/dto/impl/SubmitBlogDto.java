package com.chiu.megalith.coop.dto.impl;

import com.chiu.megalith.coop.dto.BaseDto;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.io.Serializable;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
@AllArgsConstructor
public class SubmitBlogDto extends BaseDto implements Serializable {

}
