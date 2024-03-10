package org.chiu.megalith.manage.service;


import org.chiu.megalith.manage.vo.RoleMenuVo;
import org.chiu.megalith.manage.vo.MenuVo;

import java.util.ArrayList;
import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
public interface RoleMenuService {

    List<MenuVo> getCurrentUserNav(String role);

    List<RoleMenuVo> getMenusInfo(Long roleId);

    void saveMenu(Long roleId, ArrayList<Long> menuIds);

}
