package org.chiu.megalith.security.component.provider;

import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.manage.repository.RoleRepository;
import org.springframework.security.authentication.*;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;

import static org.chiu.megalith.infra.lang.ExceptionMessage.*;

/**
 * @author mingchiuli
 * @create 2023-01-31 2:09 am
 */
public abstract sealed class ProviderBase extends DaoAuthenticationProvider permits
        EmailAuthenticationProvider,
        PasswordAuthenticationProvider,
        SMSAuthenticationProvider {

    protected String grantType;

    protected UserDetailsService userDetailsService;

    protected RoleRepository roleRepository;

    protected ProviderBase(String grantType,
                           UserDetailsService userDetailsService,
                           RoleRepository roleRepository) {
        setUserDetailsService(userDetailsService);
        setHideUserNotFoundExceptions(false);
        this.grantType = grantType;
        this.userDetailsService = userDetailsService;
        this.roleRepository = roleRepository;
    }

    protected boolean supports(String type) {
        return grantType.equals(type);
    }

    protected abstract void authProcess(UserDetails user, UsernamePasswordAuthenticationToken authentication);

    private void checkRoleStatus(UserDetails user) {
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

    @Override
    protected void additionalAuthenticationChecks(UserDetails userDetails, UsernamePasswordAuthenticationToken authentication) throws AuthenticationException {
        authProcess(userDetails, authentication);
    }

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        UserDetails user = retrieveUser(authentication.getName(), (UsernamePasswordAuthenticationToken) authentication);
        if (!user.isAccountNonLocked()) {
            throw new LockedException(ACCOUNT_LOCKED.getMsg());
        }
        checkRoleStatus(user);
        additionalAuthenticationChecks(user, (UsernamePasswordAuthenticationToken) authentication);
        return createSuccessAuthentication(user, authentication, user);
    }
}
