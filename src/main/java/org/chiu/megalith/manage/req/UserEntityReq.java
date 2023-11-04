package org.chiu.megalith.manage.req;

import org.chiu.megalith.infra.valid.ListValue;
import org.chiu.megalith.infra.valid.Phone;
import org.chiu.megalith.infra.valid.Username;
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

    @NotBlank
    private String role;
}
