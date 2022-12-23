package com.chiu.megalith.authentication.role;

import com.chiu.megalith.common.lang.Const;
import org.springframework.stereotype.Component;

/**
 * @author mingchiuli
 * @create 2022-12-23 8:24 pm
 */
@Component(value = "jwtRoleHolder")
public class JwtRoleHolder {

    private final String role = Const.TOKEN_TOOL.getMsg();

    public String getRole() {
        return this.role;
    }
}
