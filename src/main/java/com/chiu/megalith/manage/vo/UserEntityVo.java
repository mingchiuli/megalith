package com.chiu.megalith.manage.vo;

import com.chiu.megalith.base.valid.ListValue;
import com.chiu.megalith.base.valid.Phone;
import com.chiu.megalith.base.valid.Username;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UserEntityVo {

    private Long id;

    @Username
    private String username;

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
