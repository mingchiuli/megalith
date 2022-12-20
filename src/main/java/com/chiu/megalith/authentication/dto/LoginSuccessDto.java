package com.chiu.megalith.authentication.dto;

import com.chiu.megalith.authentication.user.entity.UserEntity;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class LoginSuccessDto {

    UserEntity user;

    String token;
}
