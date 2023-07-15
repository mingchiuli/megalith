package org.chiu.megalith.security.user;

import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.User;

import java.util.Collection;

/**
 * @author mingchiuli
 * @create 2023-01-14 9:19 am
 */
public class LoginUser extends User {

    public static final ThreadLocal<AuthenticationException> loginException = new ThreadLocal<>();

    public static final ThreadLocal<LoginUser> loginUserCache = new ThreadLocal<>();

    private final String grantType;

    private final Long userId;

    public LoginUser(String username,
                     String password,
                     boolean enabled,
                     boolean accountNonExpired,
                     boolean credentialsNonExpired,
                     boolean accountNonLocked,
                     Collection<? extends GrantedAuthority> authorities,
                     String grantType,
                     Long userId) {
        super(username, password, enabled, accountNonExpired, credentialsNonExpired, accountNonLocked, authorities);
        this.grantType = grantType;
        this.userId = userId;
    }

    public String getGrantType() {
        return grantType;
    }

    public Long getUserId() {
        return userId;
    }
}
