package org.chiu.megalith.user.service.impl;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.code.CodeFactory;
import org.chiu.megalith.infra.exception.CommitException;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.infra.user.UserIndexMessage;
import org.chiu.megalith.user.constant.UserOperateEnum;
import org.chiu.megalith.user.convertor.UserEntityVoConvertor;
import org.chiu.megalith.user.entity.RoleEntity;
import org.chiu.megalith.user.entity.UserEntity;
import org.chiu.megalith.user.entity.UserRoleEntity;
import org.chiu.megalith.user.event.UserOperateEvent;
import org.chiu.megalith.user.repository.RoleRepository;
import org.chiu.megalith.user.repository.UserRepository;
import org.chiu.megalith.user.repository.UserRoleRepository;
import org.chiu.megalith.user.req.UserEntityRegisterReq;
import org.chiu.megalith.user.req.UserEntityReq;
import org.chiu.megalith.user.service.UserRoleService;
import org.chiu.megalith.user.vo.UserEntityVo;
import org.chiu.megalith.user.wrapper.UserRoleWrapper;
import org.springframework.beans.BeanUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

import static org.chiu.megalith.infra.lang.Const.*;
import static org.chiu.megalith.infra.lang.Const.REGISTER_PREFIX;
import static org.chiu.megalith.infra.lang.ExceptionMessage.*;
import static org.chiu.megalith.infra.lang.StatusEnum.NORMAL;

/**
 * @Author limingjiu
 * @Date 2024/5/29 22:12
 **/
@Service
@RequiredArgsConstructor
public class UserRoleServiceImpl implements UserRoleService {

    private final RoleRepository roleRepository;

    private final CodeFactory codeFactory;

    private final StringRedisTemplate redisTemplate;

    private final UserRoleWrapper userRoleWrapper;

    private final UserRepository userRepository;

    private final PasswordEncoder passwordEncoder;

    private final ApplicationContext applicationContext;

    private final UserRoleRepository userRoleRepository;

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

    @Override
    public void saveRegisterPage(String token, UserEntityRegisterReq userEntityRegisterReq) {
        Boolean exist = redisTemplate.hasKey(REGISTER_PREFIX.getInfo() + token);
        if (Objects.isNull(exist) || Boolean.FALSE.equals(exist)) {
            throw new BadCredentialsException(NO_AUTH.getMsg());
        }
        String password = userEntityRegisterReq.getPassword();
        String confirmPassword = userEntityRegisterReq.getConfirmPassword();
        if (!Objects.equals(confirmPassword, password)) {
            throw new MissException(PASSWORD_DIFF.getMsg());
        }

        String phone = userEntityRegisterReq.getPhone();
        if (!StringUtils.hasLength(phone)) {
            String fakePhone = codeFactory.create(PHONE_CODE.getInfo());
            userEntityRegisterReq.setPhone(fakePhone);
        }

        String username = userEntityRegisterReq.getUsername();
        String usernameCopy = redisTemplate.opsForValue().get(REGISTER_PREFIX.getInfo() + token);
        if (StringUtils.hasLength(usernameCopy) && !Objects.equals(usernameCopy, username)) {
            throw new BadCredentialsException(NO_AUTH.getMsg());
        }

        UserEntityReq userEntityReq = new UserEntityReq();

        Optional<UserEntity> userEntity = userRepository.findByUsernameAndStatus(username, NORMAL.getCode());
        userEntity.ifPresent(entity -> userEntityRegisterReq.setId(entity.getId()));

        BeanUtils.copyProperties(userEntityRegisterReq, userEntityReq);
        userEntityReq.setRoles(Collections.singletonList(USER.getInfo()));
        userEntityReq.setStatus(NORMAL.getCode());
        saveOrUpdate(userEntityReq);
        redisTemplate.delete(REGISTER_PREFIX.getInfo() + token);
    }

    @Override
    public List<String> findRoleCodesByUserId(Long userId) {
        List<Long> roleIds = userRoleRepository.findByUserIdIn(Collections.singletonList(userId)).stream()
                .map(UserRoleEntity::getRoleId).toList();

        return roleRepository.findAllById(roleIds).stream()
                    .map(RoleEntity::getCode)
                    .toList();
    }

    @Override
    public UserEntityVo findById(Long userId) {
        UserEntity userEntity = userRepository.findById(userId)
                .orElseThrow(() -> new MissException(USER_NOT_EXIST));

        List<String> roleCodes = findRoleCodesByUserId(userId);
        return UserEntityVoConvertor.convert(userEntity, roleCodes);
    }

    @Override
    public PageAdapter<UserEntityVo> listPage(Integer currentPage, Integer size) {
        var pageRequest = PageRequest.of(currentPage - 1,
                size,
                Sort.by("created").ascending());
        Page<UserEntity> page = userRepository.findAll(pageRequest);
        List<Long> userId = page.get()
                .map(UserEntity::getId)
                .toList();
        List<UserRoleEntity> userRoleEntities = userRoleRepository.findByUserIdIn(userId);
        List<Long> roleIds = userRoleEntities.stream().map(UserRoleEntity::getRoleId).toList();
        Map<Long, String> idCodeMap = roleRepository.findAllById(roleIds).stream()
                .collect(Collectors.toMap(RoleEntity::getId, RoleEntity::getCode));

        Map<Long, List<String>> userIdRoleMap = new HashMap<>();
        userRoleEntities.forEach(item -> {
            Long id = item.getUserId();
            userIdRoleMap.putIfAbsent(id, new ArrayList<>());
            List<String> roles = userIdRoleMap.get(id);
            roles.add(idCodeMap.get(id));
        });

        return UserEntityVoConvertor.convert(page, userIdRoleMap);
    }

    @Override
    public void deleteUsers(List<Long> ids) {
        List<Long> userRoleIds = userRoleRepository.findByUserIdIn(ids)
                .stream()
                .map(UserRoleEntity::getId).toList();

        userRoleWrapper.deleteUsers(ids, userRoleIds);
    }
}
