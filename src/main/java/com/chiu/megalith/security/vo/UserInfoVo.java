package com.chiu.megalith.security.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author mingchiuli
 * @create 2023-04-15 2:14 pm
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UserInfoVo {

    private String nickname;

    private String avatar;


}
