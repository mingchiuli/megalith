package org.chiu.megalith.authority.vo;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class RoleAuthorityVo {

    private Long authorityId;

    private String code;

    private Boolean check;
}
