package com.chiu.megalith.manage.service;


import com.chiu.megalith.manage.entity.MenuEntity;
import com.chiu.megalith.manage.vo.MenuEntityVo;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
public interface MenuService {

    List<MenuEntityVo> getCurrentUserNav(Long userId);

    MenuEntity findById(Long id);

    List<MenuEntityVo> tree();

    void saveOrUpdate(MenuEntityVo menu);

    void delete(Long id);
}
