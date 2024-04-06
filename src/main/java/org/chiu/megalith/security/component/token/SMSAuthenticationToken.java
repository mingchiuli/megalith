package org.chiu.megalith.security.component.token;

import org.springframework.security.authentication.AbstractAuthenticationToken;

import java.io.Serial;

public class SMSAuthenticationToken extends AbstractAuthenticationToken {

    @Serial
    private static final long serialVersionUID = 6200L;
    private final Object principal;
    private final Object credentials;

    public SMSAuthenticationToken(Object principal, Object credentials) {
        super(null);
        this.principal = principal;
        this.credentials = credentials;
        this.setAuthenticated(false);
    }

    @Override
    public Object getCredentials() {
        return credentials;
    }

    @Override
    public Object getPrincipal() {
        return principal;
    }
}
