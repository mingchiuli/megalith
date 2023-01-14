package com.chiu.megalith.authentication.user;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.User;

import java.util.Collection;

/**
 * @author mingchiuli
 * @create 2023-01-14 9:19 am
 */
public class LoginUser extends User {

    private final String grantType;

    public LoginUser(String username, String password, boolean enabled, boolean accountNonExpired, boolean credentialsNonExpired, boolean accountNonLocked, Collection<? extends GrantedAuthority> authorities, String grantType) {
        super(username, password, enabled, accountNonExpired, credentialsNonExpired, accountNonLocked, authorities);
        this.grantType = grantType;
    }

    public String getGrantType() {
        return grantType;
    }
}
