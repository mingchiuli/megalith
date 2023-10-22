package org.chiu.megalith.security.component.provider;

import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.infra.utils.SpringUtils;
import org.chiu.megalith.manage.repository.RoleRepository;
import org.chiu.megalith.security.user.LoginUser;
import org.springframework.security.authentication.*;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;

import java.util.List;
import java.util.Objects;

import static org.chiu.megalith.infra.lang.ExceptionMessage.ROLE_DISABLED;
import static org.chiu.megalith.infra.lang.ExceptionMessage.ROLE_NOT_EXIST;

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

    protected RoleRepository roleRepository;

    protected ProviderSupport(String grantType,
                              UserDetailsService userDetailsService,
                              RoleRepository roleRepository) {
        setUserDetailsService(userDetailsService);
        setHideUserNotFoundExceptions(false);
        this.grantType = grantType;
        this.userDetailsService = userDetailsService;
        this.roleRepository = roleRepository;
    }

    private static class LastProvider {
        private static final AuthenticationProvider lastChainProvider;

        static {
            ProviderManager manager = SpringUtils.getBean(ProviderManager.class);
            List<AuthenticationProvider> providers = manager.getProviders();
            lastChainProvider = providers.get(providers.size() - 1);
        }
    }

    protected abstract void authProcess(LoginUser user, UsernamePasswordAuthenticationToken authentication);

    private void checkRoleStatus(LoginUser user) {
        String role = user.getAuthorities().stream()
                .findFirst()
                .map(GrantedAuthority::getAuthority)
                .orElseThrow()
                .substring(Const.ROLE_PREFIX.getInfo().length());

        Integer status = roleRepository.findByCode(role)
                .orElseThrow(() -> new BadCredentialsException(ROLE_NOT_EXIST.getMsg()))
                .getStatus();

        if (StatusEnum.HIDE.getCode().equals(status)) {
            throw new BadCredentialsException(ROLE_DISABLED.getMsg());
        }
    }

    private boolean lastProvider() {
        return LastProvider.lastChainProvider.getClass().equals(this.getClass());
    }

    @Override
    protected void additionalAuthenticationChecks(UserDetails userDetails, UsernamePasswordAuthenticationToken authentication) throws AuthenticationException {
        LoginUser user = (LoginUser) userDetails;
        if (Objects.equals(grantType, user.getGrantType())) {
            try {
                checkRoleStatus(user);
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
