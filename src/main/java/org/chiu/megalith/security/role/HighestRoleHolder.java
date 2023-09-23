package org.chiu.megalith.security.role;

import lombok.Getter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * @author mingchiuli
 * @create 2022-11-28 12:03 am
 */
@Getter
@Component(value = "highestRoleHolder")
public class HighestRoleHolder {
    
    @Value("${blog.highest-role}")
    private String role;

}
