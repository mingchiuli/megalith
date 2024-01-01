package org.chiu.megalith.manage.service;


import org.chiu.megalith.manage.vo.MenuRoleVo;
import org.chiu.megalith.manage.vo.MenuVo;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
public interface RoleMenuService {

    List<Long> getNavMenuIdsByRoleId(String role);

    List<MenuVo> getCurrentUserNav(Long userId);

    List<MenuRoleVo> getMenusInfo(Long roleId);

    List<MenuVo> tree();
}
