package org.chiu.megalith.manage.service;


import org.chiu.megalith.manage.vo.MenuDisplayVo;
import org.chiu.megalith.manage.vo.RoleAuthorityVo;
import org.chiu.megalith.manage.vo.RoleMenuVo;
import org.chiu.megalith.manage.vo.MenuVo;

import java.util.ArrayList;
import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
public interface RoleMenuService {

    List<Long> getNavMenuIdsByRoleId(String role);

    List<MenuVo> getCurrentUserNav(Long userId);

    List<RoleMenuVo> getMenusInfo(Long roleId);

    List<MenuDisplayVo> tree();

    List<RoleAuthorityVo> getAuthoritiesInfo(Long roleId);

    void saveAuthority(Long roleId, ArrayList<Long> authorityIds);
}
