package org.chiu.megalith.manage.vo;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class UserEntityVo {

    private Long id;

    private String username;

    private String nickname;

    private String avatar;

    private String email;

    private String phone;

    private Integer status;

    private LocalDateTime created;

    private LocalDateTime lastLogin;

    private String role;
}
