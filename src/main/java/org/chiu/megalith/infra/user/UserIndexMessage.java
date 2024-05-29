package org.chiu.megalith.infra.user;


import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Data;
import org.chiu.megalith.user.constant.UserOperateEnum;

@Data
@AllArgsConstructor
public class UserIndexMessage implements Serializable {

    private Long userId;

    private UserOperateEnum userOperateEnum;

}
