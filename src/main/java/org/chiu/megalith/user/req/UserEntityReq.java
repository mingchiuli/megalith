package org.chiu.megalith.user.req;

import jakarta.validation.constraints.NotEmpty;
import org.chiu.megalith.infra.vaild.ListValue;
import org.chiu.megalith.user.valid.Phone;
import org.chiu.megalith.user.valid.Username;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.util.List;

@Data
public class UserEntityReq {

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

    @NotEmpty
    private List<String> roles;
}
