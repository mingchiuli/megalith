package com.chiu.megalith.security.role;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * @author mingchiuli
 * @create 2022-11-28 12:03 am
 */
@Component(value = "highestRoleHolder")
public class HighestRoleHolder {
    @Value("${blog.highest-role}")
    private String role;

    public String getRole() {
        return this.role;
    }
}
