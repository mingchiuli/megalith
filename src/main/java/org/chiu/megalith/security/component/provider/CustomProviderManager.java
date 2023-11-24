package org.chiu.megalith.security.component.provider;

import org.springframework.security.authentication.*;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;

import java.util.List;
import java.util.Objects;

import static org.chiu.megalith.infra.lang.ExceptionMessage.INVALID_LOGIN_OPERATE;

public class CustomProviderManager extends ProviderManager {

    public CustomProviderManager(List<AuthenticationProvider> providers) {
        super(providers, null);
    }

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        Authentication result;
        for (AuthenticationProvider provider : getProviders()) {
            result = provider.authenticate(authentication);
            if (Objects.nonNull(result)) {
                return result;
            }
        }
        throw new BadCredentialsException(INVALID_LOGIN_OPERATE.getMsg());
    }
}
