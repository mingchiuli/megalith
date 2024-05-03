package org.chiu.megalith.infra.utils;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import static org.chiu.megalith.infra.lang.Const.ROLE_PREFIX;
import static org.chiu.megalith.infra.lang.ExceptionMessage.AUTH_EXCEPTION;

@Component
public class SecurityUtils {

    @Value("${blog.highest-role}")
    private String highestRole;

    private SecurityUtils(){}

    public static String getLoginRole() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Boolean.TRUE.equals(authentication instanceof AnonymousAuthenticationToken)) {
            throw new BadCredentialsException(AUTH_EXCEPTION.getMsg());
        }
        Object role = authentication.getDetails();
        return (String) role;
    }

    public static Authentication getLoginAuthentication() {
        return SecurityContextHolder.getContext().getAuthentication();
    }

    public static Long getLoginUserId() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Boolean.TRUE.equals(authentication instanceof AnonymousAuthenticationToken)) {
            throw new BadCredentialsException(AUTH_EXCEPTION.getMsg());
        }
        return Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());
    }

    public Boolean isAdmin(String role) {
        return (ROLE_PREFIX.getInfo() + highestRole).equals(role);
    }
}
