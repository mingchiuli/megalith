package org.chiu.megalith.manage.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.entity.RoleMenuEntity;
import org.chiu.megalith.manage.repository.RoleAuthorityRepository;
import org.chiu.megalith.manage.repository.RoleMenuRepository;
import org.chiu.megalith.manage.repository.RoleRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Component
@RequiredArgsConstructor
public class RoleWrapper {

    private final RoleMenuRepository roleMenuRepository;

    private final RoleRepository roleRepository;

    private final RoleAuthorityRepository roleAuthorityRepository;

    @Transactional
    public void saveMenu(Long roleId, List<RoleMenuEntity> roleMenuEntities) {
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
