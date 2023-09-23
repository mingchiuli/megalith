package org.chiu.megalith.coop.vo;

import lombok.Data;
import lombok.EqualsAndHashCode;


@EqualsAndHashCode(callSuper = true)
@Data
public class SyncContentVo extends BaseTransferVo {

    private String content;

    private Integer offset;

    private String operateType;
}
