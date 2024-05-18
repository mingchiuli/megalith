package org.chiu.megalith.infra.user;


import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class UserIndexMessage implements Serializable {

    private Long userId;

    private String role;

    private String roleLast;
}
