package org.chiu.megalith.authority.service;


import jakarta.servlet.http.HttpServletResponse;
import org.chiu.megalith.authority.req.MenuEntityReq;
import org.chiu.megalith.authority.vo.MenuDisplayVo;
import org.chiu.megalith.authority.vo.MenuEntityVo;

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
