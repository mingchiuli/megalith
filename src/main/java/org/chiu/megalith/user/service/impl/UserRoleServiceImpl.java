package org.chiu.megalith.user.service.impl;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.exception.CommitException;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.user.UserIndexMessage;
import org.chiu.megalith.user.constant.UserOperateEnum;
import org.chiu.megalith.user.entity.UserEntity;
import org.chiu.megalith.user.entity.UserRoleEntity;
import org.chiu.megalith.user.event.UserOperateEvent;
import org.chiu.megalith.user.repository.RoleRepository;
import org.chiu.megalith.user.repository.UserRepository;
import org.chiu.megalith.user.req.UserEntityReq;
import org.chiu.megalith.user.service.UserRoleService;
import org.chiu.megalith.user.wrapper.UserRoleWrapper;
import org.springframework.beans.BeanUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static org.chiu.megalith.infra.lang.ExceptionMessage.PASSWORD_REQUIRED;
import static org.chiu.megalith.infra.lang.ExceptionMessage.USER_NOT_EXIST;

/**
 * @Author limingjiu
 * @Date 2024/5/29 22:12
 **/
@Service
@RequiredArgsConstructor
public class UserRoleServiceImpl implements UserRoleService {

    private final RoleRepository roleRepository;

    private final UserRoleWrapper userRoleWrapper;

    private final UserRepository userRepository;

    private final PasswordEncoder passwordEncoder;

    private final ApplicationContext applicationContext;

    @Override
    public void saveOrUpdate(UserEntityReq userEntityReq) {
        Long id = userEntityReq.getId();
        List<String> roles = userEntityReq.getRoles();
        UserEntity userEntity;
        UserOperateEnum userOperateEnum;

        if (Objects.nonNull(id)) {
            userEntity = userRepository.findById(id)
                    .orElseThrow(() -> new MissException(USER_NOT_EXIST));

            String password = userEntityReq.getPassword();
            if (StringUtils.hasLength(password)) {
                userEntityReq.setPassword(passwordEncoder.encode(password));
            } else {
                userEntityReq.setPassword(userEntity.getPassword());
            }
            userOperateEnum = UserOperateEnum.UPDATE;
        } else {
            userEntity = new UserEntity();
            userEntityReq.setPassword(
                    passwordEncoder.encode(Optional.ofNullable(userEntityReq.getPassword())
                            .orElseThrow(() -> new CommitException(PASSWORD_REQUIRED))
                    )
            );
            userOperateEnum = UserOperateEnum.CREATE;
        }

        BeanUtils.copyProperties(userEntityReq, userEntity);

        Long userId = userEntity.getId();
        List<UserRoleEntity> userRoleEntities = new ArrayList<>();
        roleRepository.findByCodeIn(roles)
                .forEach(role -> userRoleEntities.add(UserRoleEntity.builder()
                        .userId(userId)
                        .roleId(role.getId())
                        .build()));

        userRoleWrapper.saveOrUpdate(userEntity, userRoleEntities);
        var userIndexMessage = new UserIndexMessage(userId, userOperateEnum);
        applicationContext.publishEvent(new UserOperateEvent(this, userIndexMessage));
    }
}
