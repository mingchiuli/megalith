package com.chiu.megalith.authentication.role;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * @author mingchiuli
 * @create 2022-12-01 9:36 pm
 */
@Component(value = "defaultRoleHolder")
public class DefaultRoleHolder {
    @Value("${blog.default-role}")
    private String roles;

    public String[] getRole() {
        return roles.split(",");
    }
}
