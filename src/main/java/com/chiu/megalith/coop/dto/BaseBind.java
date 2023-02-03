package com.chiu.megalith.coop.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.io.Serializable;

/**
 * @author mingchiuli
 * @create 2023-01-21 8:59 pm
 */
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class BaseBind implements Serializable {

    protected Long from;

    protected Long blogId;
}
