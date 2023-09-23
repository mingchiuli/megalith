package org.chiu.megalith.manage.vo;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class RoleEntityVo {

    private Long id;

    private String name;

    private String code;

    private String remark;

    private LocalDateTime created;

    private LocalDateTime updated;

    private Integer status;
}
