package com.chiu.megalith.manage.service.impl;

import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.repository.UserRepository;
import com.chiu.megalith.manage.service.UserService;
import com.chiu.megalith.manage.vo.UserEntityVo;
import com.chiu.megalith.base.exception.CommitException;
import com.chiu.megalith.base.exception.NotFoundException;
import com.chiu.megalith.base.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import java.time.LocalDateTime;
import java.util.List;
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
    public UserEntity retrieveUserInfo(String username) {
        return userRepository.retrieveUserInfo(username)
                .orElseThrow(() -> new UsernameNotFoundException("user not exist"));
    }

    @Override
    public void updateLoginTime(String username,
                                LocalDateTime time) {
        userRepository.updateLoginTime(username, time);
    }

    @Override
    public void saveOrUpdate(UserEntityVo userEntityVo) {
        var ref = new Object() {
            UserEntity userEntity;
        };

        LocalDateTime now = LocalDateTime.now();

        Optional.ofNullable(userEntityVo.getId()).ifPresentOrElse(id -> {
            ref.userEntity = userRepository.findById(id)
                    .orElseThrow(() -> new NotFoundException("user not exist"));

            Optional.ofNullable(userEntityVo.getPassword()).ifPresentOrElse(password ->
                    userEntityVo.setPassword(passwordEncoder.encode(password)), () ->
                    userEntityVo.setPassword(ref.userEntity.getPassword()));
        }, () -> {
            ref.userEntity = UserEntity
                    .builder()
                    .created(now)
                    .lastLogin(now)
                    .build();
            userEntityVo.setPassword(
                    passwordEncoder.encode(
                            Optional.ofNullable(userEntityVo.getPassword())
                                    .orElseThrow(() -> new CommitException("password is required"))
                    )
            );
        });

        BeanUtils.copyProperties(userEntityVo, ref.userEntity);
        userRepository.save(ref.userEntity);
    }

    @Override
    public UserEntity findById(Long userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new NotFoundException("user not exist"));
    }

    @Override
    public void changeUserStatusById(Long id, Integer status) {
        userRepository.setUserStatusById(id, status);
    }

    @Override
    public void changeUserStatusByUsername(String username,
                                 Integer status) {
        userRepository.setUserStatusByUsername(username, status);
    }

    @Override
    public PageAdapter<UserEntity> listPage(Integer currentPage,
                                            Integer size) {
        Pageable pageRequest = PageRequest.of(currentPage - 1,
                size,
                Sort.by("created").ascending());
        Page<UserEntity> page = userRepository.findAll(pageRequest);
        return new PageAdapter<>(page);
    }

    @Override
    public void deleteUsers(List<Long> ids) {
        userRepository.deleteAllById(ids);
    }

    @Override
    public UserEntity findByIdWithoutPassword(Long id) {
        UserEntity userEntity = findById(id);
        userEntity.setPassword(null);
        return userEntity;
    }

    @Override
    public List<Long> findIdsByStatus(Integer status) {
        return userRepository.findByStatus(status);
    }


}
