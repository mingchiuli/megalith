package org.chiu.megalith.user.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.user.cache.CacheEvict;
import org.chiu.megalith.user.cache.handler.AuthorityCacheEvictHandler;
import org.chiu.megalith.user.cache.handler.AllAuthorityCacheEvictHandler;
import org.chiu.megalith.user.cache.handler.AllMenuAndButtonCacheEvictHandler;
import org.chiu.megalith.user.cache.handler.MenuAndButtonCacheEvictHandler;
import org.chiu.megalith.user.entity.RoleEntity;
import org.chiu.megalith.user.repository.RoleAuthorityRepository;
import org.chiu.megalith.user.repository.RoleMenuRepository;
import org.chiu.megalith.user.repository.RoleRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Component
@RequiredArgsConstructor
public class RoleMenuAuthorityWrapper {

    private final RoleRepository roleRepository;

    private final RoleAuthorityRepository roleAuthorityRepository;

    private final RoleMenuRepository roleMenuRepository;


    @Transactional
    @CacheEvict(handler = { AllAuthorityCacheEvictHandler.class, AllMenuAndButtonCacheEvictHandler.class })
    public void delete(List<Long> ids) {
        roleRepository.deleteAllById(ids);
        roleMenuRepository.deleteAllByRoleId(ids);
        roleAuthorityRepository.deleteAllByRoleId(ids);
    }
}
