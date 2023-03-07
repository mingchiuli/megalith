package com.chiu.megalith.security.component.provider;

import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.security.user.LoginUser;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Component;

/**
 * @author mingchiuli
 * @create 2023-03-08 1:59 am
 */
@Component
public final class PhoneAuthenticationProvider extends ProviderSupport {

    public PhoneAuthenticationProvider(UserDetailsService userDetailsService) {
        super(Const.GRANT_TYPE_PHONE.getInfo(), userDetailsService);
    }

    @Override
    protected void authProcess(LoginUser user, UsernamePasswordAuthenticationToken authentication) {
        throw new BadCredentialsException("we didn't support phone sms login until now");
    }
}
