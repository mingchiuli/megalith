package org.chiu.megalith.manage.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.cache.CacheEvict;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.manage.entity.RoleMenuEntity;
import org.chiu.megalith.manage.repository.RoleAuthorityRepository;
import org.chiu.megalith.manage.repository.RoleMenuRepository;
import org.chiu.megalith.manage.repository.RoleRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Component
@RequiredArgsConstructor
public class RoleMenuWrapper {

    private final RoleMenuRepository roleMenuRepository;

    private final RoleRepository roleRepository;

    private final RoleAuthorityRepository roleAuthorityRepository;

    @Transactional
    @CacheEvict(prefix = {Const.HOT_MENUS})
    public void saveMenu(Long roleId, ArrayList<RoleMenuEntity> roleMenuEntities) {
        roleMenuRepository.deleteByRoleId(roleId);
        roleMenuRepository.saveAll(roleMenuEntities);
    }

    @Transactional
    public void delete(List<Long> ids) {
        roleRepository.deleteAllById(ids);
        roleMenuRepository.deleteAllByRoleId(ids);
        roleAuthorityRepository.deleteAllByRoleId(ids);
    }
}
