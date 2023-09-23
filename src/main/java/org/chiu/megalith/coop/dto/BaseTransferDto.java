package org.chiu.megalith.coop.dto;

import java.util.List;

import lombok.Data;

/**
 * @author mingchiuli
 * @create 2023-04-11 6:40 pm
 */
@Data
public abstract class BaseTransferDto {

    protected Long fromId;

    protected Long blogId;

    protected List<Long> toId;
}
