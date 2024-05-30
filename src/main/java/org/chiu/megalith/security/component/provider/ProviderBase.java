package org.chiu.megalith.security.component.provider;

import org.chiu.megalith.user.entity.RoleEntity;
import org.chiu.megalith.user.repository.RoleRepository;
import org.springframework.security.authentication.*;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;

import java.util.List;

import static org.chiu.megalith.infra.lang.Const.ROLE_PREFIX;
import static org.chiu.megalith.infra.lang.ExceptionMessage.*;
import static org.chiu.megalith.infra.lang.StatusEnum.NORMAL;

/**
 * @author mingchiuli
 * @create 2023-01-31 2:09 am
 */
public abstract sealed class ProviderBase extends DaoAuthenticationProvider permits
        EmailAuthenticationProvider,
        PasswordAuthenticationProvider,
        SMSAuthenticationProvider {
    protected UserDetailsService userDetailsService;

    protected RoleRepository roleRepository;

    protected ProviderBase(UserDetailsService userDetailsService,
                           RoleRepository roleRepository) {
        setUserDetailsService(userDetailsService);
        setHideUserNotFoundExceptions(false);
        this.userDetailsService = userDetailsService;
        this.roleRepository = roleRepository;
    }

    protected abstract void authProcess(UserDetails user, Authentication authentication);

    private void checkRoleStatus(UserDetails user) {
        List<String> roles = user.getAuthorities().stream()
                .map(GrantedAuthority::getAuthority)
                .map(r -> r.substring(ROLE_PREFIX.getInfo().length()))
                .toList();

        List<RoleEntity> roleEntities = roleRepository.findByCodeInAndStatus(roles, NORMAL.getCode());
        if (roleEntities.isEmpty()) {
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
