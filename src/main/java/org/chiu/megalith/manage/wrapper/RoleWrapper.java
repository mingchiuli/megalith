package org.chiu.megalith.manage.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.cache.CacheEvict;
import org.chiu.megalith.manage.cache.handler.AuthorityCacheEvictHandler;
import org.chiu.megalith.manage.cache.handler.AllAuthorityCacheEvictHandler;
import org.chiu.megalith.manage.cache.handler.AllMenuAndButtonCacheEvictHandler;
import org.chiu.megalith.manage.cache.handler.MenuAndButtonCacheEvictHandler;
import org.chiu.megalith.manage.entity.RoleEntity;
import org.chiu.megalith.manage.repository.RoleAuthorityRepository;
import org.chiu.megalith.manage.repository.RoleMenuRepository;
import org.chiu.megalith.manage.repository.RoleRepository;
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
