package org.chiu.megalith.coop.dto.impl;

import org.chiu.megalith.coop.dto.BaseDto;
import org.chiu.megalith.coop.lang.OperateType;

import lombok.*;

@EqualsAndHashCode(callSuper = true)
@Data
public class SyncContentDto extends BaseDto {

    private String content;

    private Long offset;

    private OperateType operateType;
}
