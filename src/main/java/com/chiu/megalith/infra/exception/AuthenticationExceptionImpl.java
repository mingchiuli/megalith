package com.chiu.megalith.infra.exception;


import org.springframework.security.core.AuthenticationException;

/**
 * @author mingchiuli
 * @create 2022-06-12 3:10 PM
 */
public class AuthenticationExceptionImpl extends AuthenticationException {
    public AuthenticationExceptionImpl(String message) {
        super(message);
    }
}
