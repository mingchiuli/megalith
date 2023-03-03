package com.chiu.megalith.security.component.provider;

import com.chiu.megalith.security.user.LoginUser;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;

/**
 * @author mingchiuli
 * @create 2023-01-31 2:09 am
 */
public abstract class ProviderSupport extends DaoAuthenticationProvider {

    protected abstract boolean supports(String grantType);

    protected abstract void authProcess(LoginUser user, UsernamePasswordAuthenticationToken authentication);

    protected abstract boolean lastProvider();

    @Override
    protected void additionalAuthenticationChecks(UserDetails userDetails, UsernamePasswordAuthenticationToken authentication) throws AuthenticationException {
        LoginUser user = (LoginUser) userDetails;
        if (supports(user.getGrantType())) {
            authProcess(user, authentication);
        } else {
            if (lastProvider()) {
                AuthenticationException exception = LoginUser.loginException.get();
                LoginUser.loginException.remove();
                throw exception;
            } else {
                throw new BadCredentialsException("hint:try to process next provider");
            }
        }
    }

}
