package org.chiu.megalith.infra.user;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class UserIndexMessage {

    private Long userId;

    private String role;
}
