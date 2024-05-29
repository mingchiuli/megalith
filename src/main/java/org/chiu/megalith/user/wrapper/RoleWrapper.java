package org.chiu.megalith.user.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.user.cache.CacheEvict;
import org.chiu.megalith.user.cache.handler.AuthorityCacheEvictHandler;
import org.chiu.megalith.user.cache.handler.MenuAndButtonCacheEvictHandler;
import org.chiu.megalith.user.entity.RoleEntity;
import org.chiu.megalith.user.repository.RoleRepository;
import org.springframework.stereotype.Component;

/**
 * @Author limingjiu
 * @Date 2024/5/29 19:24
 **/
@Component
@RequiredArgsConstructor
public class RoleWrapper {

    private final RoleRepository roleRepository;

    @CacheEvict(handler = { AuthorityCacheEvictHandler.class, MenuAndButtonCacheEvictHandler.class })
    public void save(RoleEntity roleEntity) {
        roleRepository.save(roleEntity);
    }
}
