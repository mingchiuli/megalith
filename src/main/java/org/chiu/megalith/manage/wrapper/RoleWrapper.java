package org.chiu.megalith.manage.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.cache.CacheEvict;
import org.chiu.megalith.infra.lang.Const;
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


    @CacheEvict(prefix = {Const.HOT_AUTHORITIES, Const.HOT_MENUS})
    public void save(RoleEntity roleEntity) {
        roleRepository.save(roleEntity);
    }


    @Transactional
    @CacheEvict(prefix = {Const.HOT_AUTHORITIES, Const.HOT_MENUS})
    public void delete(List<Long> ids) {
        roleRepository.deleteAllById(ids);
        roleMenuRepository.deleteAllByRoleId(ids);
        roleAuthorityRepository.deleteAllByRoleId(ids);
    }
}
