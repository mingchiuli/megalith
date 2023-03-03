package com.chiu.megalith.security.component.provider;

import com.chiu.megalith.common.utils.SpringUtils;
import com.chiu.megalith.security.user.LoginUser;
import org.springframework.security.authentication.*;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2023-01-31 2:09 am
 */
public abstract class ProviderSupport extends DaoAuthenticationProvider {

    private static class ProviderList {
        private static final AuthenticationProvider lastProvider;

        static {
            ProviderManager manager = SpringUtils.getBean(ProviderManager.class);
            List<AuthenticationProvider> providers = manager.getProviders();
            lastProvider = providers.get(providers.size() - 1);
        }
    }

    protected abstract boolean supports(String grantType);

    protected abstract void authProcess(LoginUser user,
                                        UsernamePasswordAuthenticationToken authentication);

    private boolean lastProvider() {
        return ProviderList.lastProvider.getClass().equals(this.getClass());
    }

    @Override
    protected void additionalAuthenticationChecks(UserDetails userDetails,
                                                  UsernamePasswordAuthenticationToken authentication) throws AuthenticationException {
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
