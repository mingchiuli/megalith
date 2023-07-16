package org.chiu.megalith.security.component.provider;

import org.chiu.megalith.infra.utils.SpringUtils;
import org.chiu.megalith.security.user.LoginUser;
import org.springframework.security.authentication.*;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;

import java.util.List;
import java.util.Objects;

/**
 * @author mingchiuli
 * @create 2023-01-31 2:09 am
 */
public abstract sealed class ProviderSupport extends DaoAuthenticationProvider permits
        EmailAuthenticationProvider,
        PasswordAuthenticationProvider,
        SMSAuthenticationProvider {

    protected String grantType;

    protected UserDetailsService userDetailsService;

    protected ProviderSupport(String grantType,
                              UserDetailsService userDetailsService) {
        setUserDetailsService(userDetailsService);
        setHideUserNotFoundExceptions(false);
        this.grantType = grantType;
        this.userDetailsService = userDetailsService;
    }

    private static class LastProvider {
        private static final AuthenticationProvider lastProvider;

        static {
            ProviderManager manager = SpringUtils.getBean(ProviderManager.class);
            List<AuthenticationProvider> providers = manager.getProviders();
            lastProvider = providers.get(providers.size() - 1);
        }
    }

    protected abstract void authProcess(LoginUser user,
                                        UsernamePasswordAuthenticationToken authentication);

    private boolean lastProvider() {
        return LastProvider.lastProvider.getClass().equals(this.getClass());
    }

    @Override
    protected void additionalAuthenticationChecks(UserDetails userDetails,
                                                  UsernamePasswordAuthenticationToken authentication) throws AuthenticationException {
        LoginUser user = (LoginUser) userDetails;
        if (Objects.equals(grantType, user.getGrantType())) {
            try {
                authProcess(user, authentication);
            } catch (AuthenticationException e) {
                if (Boolean.FALSE.equals(lastProvider())) {
                    LoginUser.loginException.set(e);
                }
                throw e;
            }
        } else {
            if (lastProvider()) {
                AuthenticationException e = LoginUser.loginException.get();
                LoginUser.loginException.remove();
                throw e;
            } else {
                throw new BadCredentialsException("hint:try to process next provider");
            }
        }
    }
}
