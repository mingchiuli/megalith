package org.chiu.megalith.manage.service;


import org.chiu.megalith.manage.entity.RoleMenuEntity;
import org.chiu.megalith.manage.vo.MenuRoleVo;
import org.chiu.megalith.manage.vo.MenuVo;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
public interface RoleMenuService {

    void deleteByRoleId(Long roleId);

    void deleteByMenuId(Long menuId);

    List<Long> findMenuIdsByRoleId(Long id);

    void saveAll(List<RoleMenuEntity> roleMenus);

    List<MenuRoleVo> setCheckMenusInfo(List<MenuVo> menusInfo, List<Long> menuIdsByRole, MenuRoleVo.MenuRoleVoBuilder parent, List<MenuRoleVo> parentChildren);
}
