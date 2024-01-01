package org.chiu.megalith.manage.service.impl;

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
import java.util.ArrayList;
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

        return UserEntityVo.builder()
                .id(userEntity.getId())
                .username(userEntity.getUsername())
                .nickname(userEntity.getNickname())
                .avatar(userEntity.getAvatar())
                .email(userEntity.getEmail())
                .phone(userEntity.getPhone())
                .status(userEntity.getStatus())
                .created(userEntity.getCreated())
                .lastLogin(userEntity.getLastLogin())
                .role(userEntity.getRole())
                .build();
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

        List<UserEntityVo> content = new ArrayList<>();
        page.getContent().forEach(user -> {
            UserEntityVo userEntityVo = UserEntityVo.builder()
                    .email(user.getEmail())
                    .phone(user.getPhone())
                    .role(user.getRole())
                    .id(user.getId())
                    .nickname(user.getNickname())
                    .status(user.getStatus())
                    .avatar(user.getAvatar())
                    .created(user.getCreated())
                    .lastLogin(user.getLastLogin())
                    .username(user.getUsername())
                    .build();
            content.add(userEntityVo);
        });

        return PageAdapter.<UserEntityVo>builder()
                .empty(page.isEmpty())
                .first(page.isFirst())
                .last(page.isLast())
                .pageNumber(page.getPageable().getPageNumber())
                .content(content)
                .totalPages(page.getTotalPages())
                .pageSize(page.getSize())
                .totalElements(page.getTotalElements())
                .build();
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

        return UserEntityVo.builder()
                .id(userEntity.getId())
                .username(userEntity.getUsername())
                .nickname(userEntity.getNickname())
                .avatar(userEntity.getAvatar())
                .email(userEntity.getEmail())
                .phone(userEntity.getPhone())
                .status(userEntity.getStatus())
                .created(userEntity.getCreated())
                .lastLogin(userEntity.getLastLogin())
                .role(userEntity.getRole())
                .build();
    }
}
