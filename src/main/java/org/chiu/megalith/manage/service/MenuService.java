package org.chiu.megalith.manage.service;


import jakarta.servlet.http.HttpServletResponse;
import org.chiu.megalith.manage.req.MenuEntityReq;
import org.chiu.megalith.manage.vo.MenuDisplayVo;
import org.chiu.megalith.manage.vo.MenuEntityVo;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
public interface MenuService {

    MenuEntityVo findById(Long id);

    void saveOrUpdate(MenuEntityReq menu);

    void delete(Long id);

    List<MenuDisplayVo> tree();

    void download(HttpServletResponse response);
}
