package com.chiu.megalith.websocket.principal;

import java.security.Principal;

/**
 * @author mingchiuli
 * @create 2022-02-23 2:07 PM
 */
public class StompPrincipal implements Principal {
    private final String userId;

    public StompPrincipal(String userId) {
        this.userId = userId;
    }

    @Override
    public String getName() {
        return userId;
    }
}
