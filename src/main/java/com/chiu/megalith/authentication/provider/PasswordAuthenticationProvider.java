package com.chiu.megalith.authentication.provider;

import com.chiu.megalith.authentication.user.LoginUser;
import com.chiu.megalith.common.lang.Const;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2023-01-14 9:02
 */
@RequiredArgsConstructor
public class PasswordAuthenticationProvider extends DaoAuthenticationProvider {

    private final PasswordEncoder passwordEncoder;

    @Override
    protected void additionalAuthenticationChecks(UserDetails userDetails,
                                                  UsernamePasswordAuthenticationToken authentication) {
        LoginUser user = (LoginUser) userDetails;

        Optional.ofNullable(authentication.getCredentials()).ifPresentOrElse(credentials -> {
            if (Const.GRANT_TYPE_PASSWORD.getMsg().equals(user.getGrantType())) {
                String presentedPassword = credentials.toString();
                if (!this.passwordEncoder.matches(presentedPassword, userDetails.getPassword())) {
                    LoginUser.loginException.set("Failed to authenticate since password does not match stored value");
                    throw new BadCredentialsException("Bad credentials");
                }
            } else {
                throw new BadCredentialsException("go next provider");
            }
        }, () -> {
            LoginUser.loginException.set("Failed to authenticate since no credentials provided");
            throw new BadCredentialsException("Bad credentials");
        });
    }
}
