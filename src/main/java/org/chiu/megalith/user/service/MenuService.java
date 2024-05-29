package org.chiu.megalith.user.service;


import jakarta.servlet.http.HttpServletResponse;
import org.chiu.megalith.user.req.MenuEntityReq;
import org.chiu.megalith.user.vo.MenuDisplayVo;
import org.chiu.megalith.user.vo.MenuEntityVo;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
public interface MenuService {

    MenuEntityVo findById(Long id);

    void saveOrUpdate(MenuEntityReq menu);

    List<MenuDisplayVo> tree();

    void download(HttpServletResponse response);
}
