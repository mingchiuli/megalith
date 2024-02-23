package org.chiu.megalith.infra.utils;

import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.Objects;

import static org.chiu.megalith.infra.lang.ExceptionMessage.AUTH_EXCEPTION;

public class SecurityUtils {

    private SecurityUtils(){}

    public static String getLoginRole() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
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
        if (Objects.isNull(authentication)) {
            throw new BadCredentialsException(AUTH_EXCEPTION.getMsg());
        }
        return Long.valueOf(SecurityContextHolder.getContext().getAuthentication().getName());
    }
}
