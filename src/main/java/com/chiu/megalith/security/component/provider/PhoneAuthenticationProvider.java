package com.chiu.megalith.security.component.provider;

import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.security.user.LoginUser;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;

/**
 * @author mingchiuli
 * @create 2023-03-08 1:59 am
 */
public final class PhoneAuthenticationProvider extends ProviderSupport {
    @Override
    protected boolean supports(String grantType) {
        return Const.GRANT_TYPE_PHONE.getInfo().equals(grantType);
    }

    @Override
    protected void authProcess(LoginUser user, UsernamePasswordAuthenticationToken authentication) {
        throw new BadCredentialsException("we didn't support phone sms login until now");
    }
}
