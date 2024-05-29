package org.chiu.megalith.infra.utils;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.user.service.RoleAuthorityService;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.web.authentication.preauth.PreAuthenticatedAuthenticationToken;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

import static org.chiu.megalith.infra.lang.Const.ROLE_PREFIX;

@Component
@RequiredArgsConstructor
public class SecurityAuthenticationUtils {

    private final RoleAuthorityService roleAuthorityService;

    public Authentication getAuthentication(List<String> roles, String userId) {
        List<String> rawRoles = new ArrayList<>();
        roles.forEach(role -> rawRoles.add(role.substring(ROLE_PREFIX.getInfo().length())));
        List<String> authorities = roleAuthorityService.getAuthoritiesByRoleCodes(rawRoles);
        PreAuthenticatedAuthenticationToken authenticationToken = new PreAuthenticatedAuthenticationToken(userId, null, AuthorityUtils.createAuthorityList(authorities));
        authenticationToken.setDetails(rawRoles);

        return authenticationToken;
    }
}
