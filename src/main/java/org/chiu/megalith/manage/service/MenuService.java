package org.chiu.megalith.manage.service;


import org.chiu.megalith.manage.req.MenuEntityReq;
import org.chiu.megalith.manage.vo.MenuEntityVo;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
public interface MenuService {

    MenuEntityVo findById(Long id);

    void saveOrUpdate(MenuEntityReq menu);

    void delete(Long id);
}
