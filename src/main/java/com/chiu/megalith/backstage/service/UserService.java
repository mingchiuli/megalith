package com.chiu.megalith.backstage.service;


import com.chiu.megalith.backstage.entity.UserEntity;
import com.chiu.megalith.backstage.vo.UserEntityVo;

import java.time.LocalDateTime;

/**
 * @author mingchiuli
 * @create 2022-12-04 4:55 pm
 */
public interface UserService {

    UserEntity retrieveUserInfo(String username);

    void updateLoginTime(String username, LocalDateTime time);

    void saveOrUpdate(UserEntityVo userEntityVo);

    UserEntity findById(Long userId);

}
