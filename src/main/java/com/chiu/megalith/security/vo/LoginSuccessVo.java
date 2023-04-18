package com.chiu.megalith.security.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author mingchiuli
 * @create 2023-04-19 1:47 am
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class LoginSuccessVo {

    private String accessToken;

    private String refreshToken;
}
