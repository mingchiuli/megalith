package org.chiu.megalith.security.token;

import lombok.Data;

@Data
public class Claims {

    private String userId;

    private String role;
}
