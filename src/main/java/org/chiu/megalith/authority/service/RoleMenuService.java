package org.chiu.megalith.authority.service;


import org.chiu.megalith.authority.vo.MenusAndButtonsVo;
import org.chiu.megalith.authority.vo.RoleMenuVo;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
public interface RoleMenuService {

    MenusAndButtonsVo getCurrentUserNav(String role);

    List<RoleMenuVo> getMenusInfo(Long roleId);

    void saveMenu(Long roleId, List<Long> menuIds);

}
