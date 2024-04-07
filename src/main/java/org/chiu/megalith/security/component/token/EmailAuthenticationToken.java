package org.chiu.megalith.security.component.token;

import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;

import java.io.Serial;

public class EmailAuthenticationToken extends UsernamePasswordAuthenticationToken {
    @Serial
    private static final long serialVersionUID = 6201L;

    public EmailAuthenticationToken(Object principal, Object credentials) {
        super(principal, credentials);
    }
}