package com.chiu.megalith.authentication.user.service.impl;

import com.chiu.megalith.authentication.user.entity.UserEntity;
import com.chiu.megalith.authentication.user.repository.UserRepository;
import com.chiu.megalith.authentication.user.service.UserService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import java.time.LocalDateTime;

/**
 * @author mingchiuli
 * @create 2022-12-04 4:55 pm
 */
@Service
@RequiredArgsConstructor
public class UserServiceImpl implements UserService {

    private final UserRepository userRepository;

    @Override
    public UserEntity findUsernameById(Long userId) {
        return userRepository.findUsernameById(userId).orElseGet(() -> new UserEntity("anonymous"));
    }

    @Override
    public UserEntity findByUsername(String username) {
        return userRepository.findByUsername(username).orElseThrow(() -> new UsernameNotFoundException("username not exist"));
    }

    @Override
    public UserEntity retrieveUserInfo(String username) {
        return userRepository.retrieveUserInfo(username).orElseThrow(() -> new UsernameNotFoundException("user not exist"));
    }

    @Override
    public void updateLoginTime(String username, LocalDateTime time) {
        userRepository.updateLoginTime(username, time);
    }

}
