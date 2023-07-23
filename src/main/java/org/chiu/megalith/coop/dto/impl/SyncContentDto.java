package org.chiu.megalith.coop.dto.impl;

import org.chiu.megalith.coop.dto.BaseDto;
import lombok.*;

@EqualsAndHashCode(callSuper = true)
@Data
public class SyncContentDto extends BaseDto {

    private String content;

    private Long offset;
}
