package org.chiu.megalith.user.service;

import org.chiu.megalith.user.req.UserEntityReq;

/**
 * @Author limingjiu
 * @Date 2024/5/29 22:12
 **/
public interface UserRoleService {

    void saveOrUpdate(UserEntityReq userEntityReq);

}
