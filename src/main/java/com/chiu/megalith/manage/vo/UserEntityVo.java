package com.chiu.megalith.manage.vo;

import com.chiu.megalith.infra.valid.ListValue;
import com.chiu.megalith.infra.valid.Phone;
import com.chiu.megalith.infra.valid.Username;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class UserEntityVo {

    private Long id;

    @Username
    private String username;

    @NotBlank
    private String nickname;

    @NotBlank
    private String avatar;

    private String password;

    @Email
    private String email;

    @Phone
    private String phone;

    @ListValue(values = {0, 1})
    private Integer status;

    @NotBlank
    private String role;
}
