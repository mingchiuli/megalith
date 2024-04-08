package org.chiu.megalith.manage.req;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.chiu.megalith.infra.valid.Phone;
import org.chiu.megalith.infra.valid.Username;

@Data
public class UserEntityRegisterReq {

    private Long id;

    @Username
    private String username;

    @NotBlank
    private String nickname;

    private String avatar;

    @NotBlank
    private String password;

    @Email
    private String email;

    @Phone
    private String phone;
}
