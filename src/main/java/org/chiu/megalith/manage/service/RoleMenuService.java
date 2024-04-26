package org.chiu.megalith.manage.service;


import org.chiu.megalith.manage.vo.MenusAndButtons;
import org.chiu.megalith.manage.vo.RoleMenuVo;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
public interface RoleMenuService {

    MenusAndButtons getCurrentUserNav(String role);

    List<RoleMenuVo> getMenusInfo(Long roleId);

    void saveMenu(Long roleId, List<Long> menuIds);

}
