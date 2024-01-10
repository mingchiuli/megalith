package org.chiu.megalith.security.component.provider;

import org.springframework.security.authentication.*;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;

import java.util.List;

import static org.chiu.megalith.infra.lang.Const.*;
import static org.chiu.megalith.infra.lang.ExceptionMessage.INVALID_LOGIN_OPERATE;

public class CustomProviderManager extends ProviderManager {

    public CustomProviderManager(List<AuthenticationProvider> providers) {
        super(providers, null);
    }

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        String username = authentication.getName();
        String grantType = getAuthGrantType(username);

        for (AuthenticationProvider provider : getProviders()) {
            if (!supports(grantType, (ProviderBase) provider)) {
                continue;
            }

            return provider.authenticate(authentication);
        }
        throw new BadCredentialsException(INVALID_LOGIN_OPERATE.getMsg());
    }

    private String getAuthGrantType(String username) {
        if (username.contains("@")) {
            return GRANT_TYPE_EMAIL.getInfo();
        } else if (username.matches("\\d+")) {
            return GRANT_TYPE_PHONE.getInfo();
        } else {
            return GRANT_TYPE_PASSWORD.getInfo();
        }
    }

    private boolean supports(String grantType, ProviderBase provider) {
        return provider.supports(grantType);
    }
}
