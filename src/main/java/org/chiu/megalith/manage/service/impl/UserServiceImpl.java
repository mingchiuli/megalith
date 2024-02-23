package org.chiu.megalith.manage.service.impl;

import org.chiu.megalith.infra.cache.CacheEvict;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.manage.convertor.UserEntityVoConvertor;
import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.repository.UserRepository;
import org.chiu.megalith.manage.service.UserService;
import org.chiu.megalith.manage.req.UserEntityReq;
import org.chiu.megalith.infra.exception.CommitException;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.vo.UserEntityVo;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static org.chiu.megalith.infra.lang.ExceptionMessage.*;

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
    public void updateLoginTime(String username, LocalDateTime time) {
        userRepository.updateLoginTime(username, time);
    }

    @Override
    @CacheEvict(prefix = Const.HOT_AUTHORITIES)
    public void saveOrUpdate(UserEntityReq userEntityReq) {
        Long id = userEntityReq.getId();
        UserEntity userEntity;
        var now = LocalDateTime.now();

        if (Objects.nonNull(id)) {
            userEntity = userRepository.findById(id)
                    .orElseThrow(() -> new MissException(USER_NOT_EXIST));

            String password = userEntityReq.getPassword();
            if (StringUtils.hasLength(password)) {
                userEntityReq.setPassword(passwordEncoder.encode(password));
            } else {
                userEntityReq.setPassword(userEntity.getPassword());
            }
        } else {
            userEntity = UserEntity.builder()
                    .created(now)
                    .lastLogin(now)
                    .build();
            userEntityReq.setPassword(
                    passwordEncoder.encode(Optional.ofNullable(userEntityReq.getPassword())
                                    .orElseThrow(() -> new CommitException(PASSWORD_REQUIRED))
                    )
            );
        }

        BeanUtils.copyProperties(userEntityReq, userEntity);
        userRepository.save(userEntity);
    }

    @Override
    public UserEntityVo findById(Long userId) {
        UserEntity userEntity = userRepository.findById(userId)
                .orElseThrow(() -> new MissException(USER_NOT_EXIST));

        return UserEntityVoConvertor.convert(userEntity);
    }

    @Override
    public void changeUserStatusByUsername(String username, Integer status) {
        userRepository.setUserStatusByUsername(username, status);
    }

    @Override
    public PageAdapter<UserEntityVo> listPage(Integer currentPage, Integer size) {
        var pageRequest = PageRequest.of(currentPage - 1,
                size,
                Sort.by("created").ascending());
        Page<UserEntity> page = userRepository.findAll(pageRequest);

        return UserEntityVoConvertor.convert(page);
    }

    @Override
    public void deleteUsers(List<Long> ids) {
        userRepository.deleteAllById(ids);
    }

    @Override
    public List<Long> findIdsByStatus(Integer status) {
        return userRepository.findByStatus(status);
    }

    @Override
    public UserEntityVo findByEmail(String email) {
        UserEntity userEntity = userRepository.findByEmail(email)
                .orElseThrow(() -> new MissException(EMAIL_NOT_EXIST));

        return UserEntityVoConvertor.convert(userEntity);
    }
}
