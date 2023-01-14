package com.chiu.megalith.authentication.provider;

import com.chiu.megalith.authentication.user.LoginUser;
import com.chiu.megalith.common.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.password.PasswordEncoder;

/**
 * @author mingchiuli
 * @create 2023-01-14 9:02 am
 */
@RequiredArgsConstructor
public class PasswordAuthenticationProvider extends DaoAuthenticationProvider {

    public PasswordAuthenticationProvider(PasswordEncoder passwordEncoder) {
        super.setPasswordEncoder(passwordEncoder);
    }

    @Override
    protected void additionalAuthenticationChecks(UserDetails userDetails,
                                                  UsernamePasswordAuthenticationToken authentication) throws AuthenticationException {
        LoginUser user = (LoginUser) userDetails;
        String grantType = user.getGrantType();
        if (Const.GRANT_TYPE_PASSWORD.getMsg().equals(grantType)) {
            super.additionalAuthenticationChecks(userDetails, authentication);
        } else {
            throw new BadCredentialsException("go next provider");
        }
    }
}
