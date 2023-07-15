package org.chiu.megalith.coop.dto.impl;

import org.chiu.megalith.coop.dto.BaseDto;
import lombok.*;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
@AllArgsConstructor
public class SyncBlogDto extends BaseDto {

    private String content;
}
