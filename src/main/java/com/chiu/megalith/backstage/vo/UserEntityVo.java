package com.chiu.megalith.backstage.vo;

import com.chiu.megalith.common.valid.ListValue;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class UserEntityVo {

    private Long id;

    @NotBlank
    private String username;

    @NotBlank
    private String avatar;

    private String password;

    @Email
    private String email;

    @ListValue(values = {0, 1})
    private Integer status;

    @NotBlank
    private String role;
}
