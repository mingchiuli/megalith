package org.chiu.megalith.infra.utils;

import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

public class SecurityUtils {

    public static String getLoginAuthority() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        return authentication.getAuthorities().stream()
                .findFirst()
                .map(GrantedAuthority::getAuthority)
                .orElseThrow(() -> new BadCredentialsException("role exception"));
    }

    public static Authentication getLoginAuthentication() {
        return SecurityContextHolder.getContext().getAuthentication();
    }

    public static Long getLoginUserId() {
        return Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());
    }
}
