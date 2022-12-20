package com.chiu.megalith.backstage.service;


import com.chiu.megalith.backstage.entity.RoleMenuEntity;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
public interface RoleMenuService {

    void deleteByRoleId(Long roleId);

    List<Long> findMenuIdsByRoleId(Long id);

    void saveAll(List<RoleMenuEntity> roleMenus);

}
