package com.chiu.megalith.auth.component.provider;

import com.chiu.megalith.auth.user.LoginUser;
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
public class PasswordAuthenticationProvider extends DaoAuthenticationProvider implements ProviderSupport {

    private final PasswordEncoder passwordEncoder;

    @Override
    protected void additionalAuthenticationChecks(UserDetails userDetails,
                                                  UsernamePasswordAuthenticationToken authentication) {
        mainProcess(userDetails, authentication);
    }

    @Override
    public boolean supports(String grantType) {
        return Const.GRANT_TYPE_PASSWORD.getInfo().equals(grantType);
    }

    @Override
    public void authProcess(LoginUser user,
                            UsernamePasswordAuthenticationToken authentication) {

        Optional.ofNullable(authentication.getCredentials()).ifPresentOrElse(credentials -> {
            String presentedPassword = credentials.toString();
            if (!passwordEncoder.matches(presentedPassword, user.getPassword())) {
                BadCredentialsException exception = new BadCredentialsException("Failed to authenticate since password does not match stored value");
                LoginUser.loginException.set(exception);
                throw exception;
            }
        }, () -> {
            BadCredentialsException exception = new BadCredentialsException("Failed to authenticate since no credentials provided");
            LoginUser.loginException.set(exception);
            throw exception;
        });
    }

    @Override
    public void mismatchProcess() {
        mismatchProcess(false);
    }
}
