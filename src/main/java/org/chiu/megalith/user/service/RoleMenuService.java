package org.chiu.megalith.user.service;


import org.chiu.megalith.user.vo.MenusAndButtonsVo;
import org.chiu.megalith.user.vo.RoleMenuVo;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
public interface RoleMenuService {

    MenusAndButtonsVo getCurrentUserNav(List<String> roles);

    List<RoleMenuVo> getMenusInfo(Long roleId);

    void saveMenu(Long roleId, List<Long> menuIds);

    void delete(Long id);

}
