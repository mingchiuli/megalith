package org.chiu.megalith.infra.utils;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.authority.service.RoleAuthorityService;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.web.authentication.preauth.PreAuthenticatedAuthenticationToken;
import org.springframework.stereotype.Component;

import java.util.List;

import static org.chiu.megalith.infra.lang.Const.ROLE_PREFIX;

@Component
@RequiredArgsConstructor
public class SecurityAuthenticationUtils {

    private final RoleAuthorityService roleAuthorityService;

    public Authentication getAuthentication(String role, String userId) {
        String rawRole = role.substring(ROLE_PREFIX.getInfo().length());
        List<String> authorities = roleAuthorityService.getAuthoritiesByRoleCode(rawRole);
        PreAuthenticatedAuthenticationToken authenticationToken = new PreAuthenticatedAuthenticationToken(userId, null, AuthorityUtils.createAuthorityList(authorities));
        authenticationToken.setDetails(role);

        return authenticationToken;
    }
}
