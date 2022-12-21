package com.chiu.megalith.backstage.service.impl;

import com.chiu.megalith.backstage.entity.UserEntity;
import com.chiu.megalith.backstage.repository.UserRepository;
import com.chiu.megalith.backstage.service.UserService;
import com.chiu.megalith.backstage.vo.UserEntityVo;
import com.chiu.megalith.common.exception.NotFoundException;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.BeanUtils;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import java.time.LocalDateTime;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-12-04 4:55 pm
 */
@Service
@RequiredArgsConstructor
public class UserServiceImpl implements UserService {

    private final PasswordEncoder passwordEncoder;

    private final UserRepository userRepository;

    @Override
    public UserEntity findUsernameById(Long userId) {
        return userRepository.findUsernameById(userId).orElseGet(() -> new UserEntity("anonymous"));
    }

    @Override
    public UserEntity retrieveUserInfo(String username) {
        return userRepository.retrieveUserInfo(username).orElseThrow(() -> new UsernameNotFoundException("user not exist"));
    }

    @Override
    public void updateLoginTime(String username, LocalDateTime time) {
        userRepository.updateLoginTime(username, time);
    }

    @Override
    public void saveOrUpdate(UserEntityVo userEntityVo) {
        var ref = new Object() {
            UserEntity userEntity;
        };
        LocalDateTime now = LocalDateTime.now();

        Optional.ofNullable(userEntityVo.getId()).ifPresentOrElse(id ->
                ref.userEntity = userRepository.findById(id).orElseThrow(() -> new NotFoundException("user not exist")),
                () -> ref.userEntity = UserEntity.builder().
                created(now).
                lastLogin(now).
                build());

        Optional.ofNullable(userEntityVo.getPassword()).ifPresentOrElse(password ->
                userEntityVo.setPassword(passwordEncoder.encode(password)),
                () -> userEntityVo.setPassword(ref.userEntity.getPassword()));


        BeanUtils.copyProperties(userEntityVo, ref.userEntity);
        userRepository.save(ref.userEntity);
    }

}
