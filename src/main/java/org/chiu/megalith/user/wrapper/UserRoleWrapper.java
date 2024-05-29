package org.chiu.megalith.user.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.user.entity.UserEntity;
import org.chiu.megalith.user.entity.UserRoleEntity;
import org.chiu.megalith.user.repository.UserRepository;
import org.chiu.megalith.user.repository.UserRoleRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * @Author limingjiu
 * @Date 2024/5/29 13:41
 **/
@Component
@RequiredArgsConstructor
public class UserRoleWrapper {

    private final UserRepository userRepository;

    private final UserRoleRepository userRoleRepository;

    @Transactional
    public void saveOrUpdate(UserEntity userEntity, List<UserRoleEntity> userRoleEntities) {
        userRepository.save(userEntity);
        userRoleRepository.saveAll(userRoleEntities);
    }
}
