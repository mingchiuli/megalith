package org.chiu.megalith.security.utils;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import java.util.List;

import static org.chiu.megalith.infra.lang.ExceptionMessage.AUTH_EXCEPTION;

@Component
public class SecurityUtils {

    @Value("${blog.highest-role}")
    private String highestRole;

    private SecurityUtils(){}

    @SuppressWarnings("unchecked")
    public static List<String> getLoginRole() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Boolean.TRUE.equals(authentication instanceof AnonymousAuthenticationToken)) {
            throw new BadCredentialsException(AUTH_EXCEPTION.getMsg());
        }

        return (List<String>) authentication.getDetails();
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

    public Boolean isAdmin(List<String> roles) {
        return roles.contains(highestRole);
    }
}
