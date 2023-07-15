package org.chiu.megalith.security.vo;

import lombok.Builder;
import lombok.Data;

/**
 * @author mingchiuli
 * @create 2023-04-19 1:47 am
 */
@Data
@Builder
public class LoginSuccessVo {

    private String accessToken;

    private String refreshToken;
}
