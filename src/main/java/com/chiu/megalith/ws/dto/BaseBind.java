package com.chiu.megalith.ws.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

/**
 * @author mingchiuli
 * @create 2023-01-21 8:59 pm
 */
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class BaseBind {

    protected Long from;

    protected Long blogId;
}
