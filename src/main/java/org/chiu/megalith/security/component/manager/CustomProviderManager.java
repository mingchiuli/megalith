package org.chiu.megalith.security.component.manager;

import org.chiu.megalith.security.component.token.EmailAuthenticationToken;
import org.chiu.megalith.security.component.token.SMSAuthenticationToken;
import org.springframework.security.authentication.*;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;

import java.util.List;

import static org.chiu.megalith.infra.lang.ExceptionMessage.INVALID_LOGIN_OPERATE;

public class CustomProviderManager extends ProviderManager {

    public CustomProviderManager(List<AuthenticationProvider> providers) {
        super(providers);
    }

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {

        authentication = getAuthGrantTypeToken(authentication);

        for (AuthenticationProvider provider : getProviders()) {
            if (provider.supports(authentication.getClass())) {
                return provider.authenticate(authentication);
            }
        }
        throw new BadCredentialsException(INVALID_LOGIN_OPERATE.getMsg());
    }

    private Authentication getAuthGrantTypeToken(Authentication authentication) {
        String username = authentication.getName();

        if (username.contains("@")) {
            return new EmailAuthenticationToken(username, authentication.getCredentials());
        } else if (username.matches("\\d+")) {
            return new SMSAuthenticationToken(username, authentication.getCredentials());
        } else {
            return authentication;
        }
    }
}
