package com.chiu.megalith.coop.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

/**
 * @author mingchiuli
 * @create 2023-04-11 6:40 pm
 */
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public abstract class BaseDto {

    protected Long fromId;

    protected Long blogId;

    protected Long toId;
}
