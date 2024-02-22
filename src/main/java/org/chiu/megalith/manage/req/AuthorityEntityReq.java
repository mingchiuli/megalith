package org.chiu.megalith.manage.req;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;


@Data
public class AuthorityEntityReq {

    private Long id;

    @NotBlank
    private String name;

    @NotBlank
    private String code;

    @NotBlank
    private String remark;

    private Integer status;
}
