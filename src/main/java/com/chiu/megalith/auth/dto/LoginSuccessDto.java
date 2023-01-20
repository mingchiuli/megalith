package com.chiu.megalith.auth.dto;

import com.chiu.megalith.manage.entity.UserEntity;
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