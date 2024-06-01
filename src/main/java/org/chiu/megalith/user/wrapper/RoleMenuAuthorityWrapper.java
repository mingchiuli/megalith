package org.chiu.megalith.user.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.user.cache.CacheEvict;
import org.chiu.megalith.user.cache.handler.BatchAuthorityCacheEvictHandler;
import org.chiu.megalith.user.cache.handler.BatchMenuAndButtonCacheEvictHandler;
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
    @CacheEvict(handler = { BatchAuthorityCacheEvictHandler.class, BatchMenuAndButtonCacheEvictHandler.class })
    public void delete(List<Long> ids) {
        roleRepository.deleteAllById(ids);
        roleMenuRepository.deleteAllByRoleId(ids);
        roleAuthorityRepository.deleteAllByRoleId(ids);
    }
}
