package com.chiu.megalith.manage.service.impl;

import com.chiu.megalith.manage.entity.RoleMenuEntity;
import com.chiu.megalith.manage.repository.RoleMenuRepository;
import com.chiu.megalith.manage.service.RoleMenuService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:26 am
 */
@Service
@RequiredArgsConstructor
public class RoleMenuServiceImpl implements RoleMenuService {

    private final RoleMenuRepository roleMenuRepository;

    @Override
    public void deleteByRoleId(Long roleId) {
        roleMenuRepository.deleteByRoleId(roleId);
    }

    @Override
    public List<Long> findMenuIdsByRoleId(Long id) {
        return roleMenuRepository.findMenuIdsByRoleId(id);
    }

    @Override
    public void saveAll(List<RoleMenuEntity> roleMenus) {
        roleMenuRepository.saveAll(roleMenus);
    }
}
