package com.chiu.megalith.auth.component.provider;

import com.chiu.megalith.auth.user.LoginUser;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.userdetails.UserDetails;

/**
 * @author mingchiuli
 * @create 2023-01-31 2:09 am
 */
public interface ProviderSupport {
    boolean supports(String grantType);

    void authProcess(LoginUser user, UsernamePasswordAuthenticationToken authentication);

    void mismatchProcess();

    default void providerProcess(UserDetails userDetails,
                                 UsernamePasswordAuthenticationToken authentication) {
        LoginUser user = (LoginUser) userDetails;
        if (supports(user.getGrantType())) {
            authProcess(user, authentication);
        } else {
            mismatchProcess();
        }
    }
}
