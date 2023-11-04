package org.chiu.megalith.infra.utils;

import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.Objects;

import static org.chiu.megalith.infra.lang.ExceptionMessage.AUTH_EXCEPTION;
import static org.chiu.megalith.infra.lang.ExceptionMessage.ROLE_EXCEPTION;

public class SecurityUtils {

    private SecurityUtils(){}

    public static String getLoginAuthority() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            throw new BadCredentialsException(AUTH_EXCEPTION.getMsg());
        }
        return authentication.getAuthorities().stream()
                .findFirst()
                .map(GrantedAuthority::getAuthority)
                .orElseThrow(() -> new BadCredentialsException(ROLE_EXCEPTION.getMsg()));
    }

    public static Authentication getLoginAuthentication() {
        return SecurityContextHolder.getContext().getAuthentication();
    }

    public static Long getLoginUserId() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            throw new BadCredentialsException(AUTH_EXCEPTION.getMsg());
        }
        return Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());
    }
}
