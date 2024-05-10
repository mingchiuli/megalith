package org.chiu.megalith.authority.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.authority.cache.CacheEvict;
import org.chiu.megalith.authority.cache.handler.AuthorityCacheEvictHandler;
import org.chiu.megalith.authority.cache.handler.AllAuthorityCacheEvictHandler;
import org.chiu.megalith.authority.cache.handler.AllMenuAndButtonCacheEvictHandler;
import org.chiu.megalith.authority.cache.handler.MenuAndButtonCacheEvictHandler;
import org.chiu.megalith.authority.entity.RoleEntity;
import org.chiu.megalith.authority.repository.RoleAuthorityRepository;
import org.chiu.megalith.authority.repository.RoleMenuRepository;
import org.chiu.megalith.authority.repository.RoleRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Component
@RequiredArgsConstructor
public class RoleWrapper {

    private final RoleRepository roleRepository;

    private final RoleAuthorityRepository roleAuthorityRepository;

    private final RoleMenuRepository roleMenuRepository;


    @CacheEvict(handler = { AuthorityCacheEvictHandler.class, MenuAndButtonCacheEvictHandler.class })
    public void save(RoleEntity roleEntity) {
        roleRepository.save(roleEntity);
    }


    @Transactional
    @CacheEvict(handler = { AllAuthorityCacheEvictHandler.class, AllMenuAndButtonCacheEvictHandler.class })
    public void delete(List<Long> ids) {
        roleRepository.deleteAllById(ids);
        roleMenuRepository.deleteAllByRoleId(ids);
        roleAuthorityRepository.deleteAllByRoleId(ids);
    }
}
