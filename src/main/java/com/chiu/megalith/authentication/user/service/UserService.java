package com.chiu.megalith.authentication.user.service;


import com.chiu.megalith.authentication.user.entity.UserEntity;

import java.time.LocalDateTime;

/**
 * @author mingchiuli
 * @create 2022-12-04 4:55 pm
 */
public interface UserService {

    UserEntity findUsernameById(Long userId);

    UserEntity findByUsername(String username);

    UserEntity retrieveUserInfo(String username);

    void updateLoginTime(String username, LocalDateTime time);


}
