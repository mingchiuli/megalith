package org.chiu.megalith.manage.service;


import org.chiu.megalith.manage.req.MenuEntityReq;
import org.chiu.megalith.manage.vo.MenuEntityVo;
import org.chiu.megalith.manage.vo.MenuVo;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
public interface MenuService {

    List<MenuVo> getCurrentUserNav(Long userId);

    MenuEntityVo findById(Long id);

    List<MenuVo> tree();

    void saveOrUpdate(MenuEntityReq menu);

    void delete(Long id);

    List<MenuVo> buildMenu(List<Long> menuIds, Boolean status);

    List<MenuVo> getNormalMenusInfo();
}
