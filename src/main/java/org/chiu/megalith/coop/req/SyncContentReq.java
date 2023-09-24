package org.chiu.megalith.coop.req;

import lombok.Data;
import lombok.EqualsAndHashCode;
import org.chiu.megalith.coop.vo.BaseTransferVo;


@EqualsAndHashCode(callSuper = true)
@Data
public class SyncContentReq extends BaseTransferVo {

    private String content;

    private Integer offset;

    private String operateType;
}
