package org.chiu.megalith.security.vo;

import lombok.Builder;
import lombok.Data;

/**
 * @author mingchiuli
 * @create 2023-04-15 2:14 pm
 */

@Data
@Builder
public class UserInfoVo {

    private String nickname;

    private String avatar;
}
