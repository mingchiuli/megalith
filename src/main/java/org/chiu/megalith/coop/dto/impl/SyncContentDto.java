package org.chiu.megalith.coop.dto.impl;

import org.chiu.megalith.coop.dto.BaseTransferDto;

import lombok.*;

@EqualsAndHashCode(callSuper = true)
@Data
public class SyncContentDto extends BaseTransferDto {

    private String content;

    private Integer offset;

    private String operateType;
}
