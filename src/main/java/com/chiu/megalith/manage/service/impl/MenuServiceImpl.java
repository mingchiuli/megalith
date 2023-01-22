package com.chiu.megalith.manage.service.impl;

import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.service.UserService;
import com.chiu.megalith.manage.entity.MenuEntity;
import com.chiu.megalith.manage.repository.MenuRepository;
import com.chiu.megalith.manage.service.MenuService;
import com.chiu.megalith.manage.service.RoleService;
import com.chiu.megalith.manage.vo.MenuEntityVo;
import com.chiu.megalith.common.exception.NotFoundException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
@Service
@RequiredArgsConstructor
public class MenuServiceImpl implements MenuService {

    private final UserService userService;

    private final RoleService roleService;

    private final MenuRepository menuRepository;

    @Override
    public List<MenuEntityVo> getCurrentUserNav(Long userId) {

        UserEntity userEntity = userService.findById(userId);
        String role = userEntity.getRole();

        List<Long> menuIds = roleService.getNavMenuIds(role);
        List<MenuEntity> menus = menuRepository.findAllById(menuIds);

        List<MenuEntityVo> menuEntityVos = menus.
                stream().
                map(menu ->
                        MenuEntityVo.
                        builder().
                        menuId(menu.
                                getMenuId()).
                        parentId(menu.
                                getParentId()).
                        icon(menu.
                                getIcon()).
                        url(menu.
                                getUrl()).
                        title(menu.
                                getTitle()).
                        name(menu.
                                getName()).
                        component(menu.
                                getComponent()).
                        type(menu.
                                getType()).
                        orderNum(menu.
                                getOrderNum()).
                        status(menu.
                                getStatus()).
                        build()).
                toList();

        // 转树状结构
        return buildTreeMenu(menuEntityVos);
    }

    @Override
    public MenuEntity findById(Long id) {
        return menuRepository.findById(id).
                orElseThrow(() -> new NotFoundException("menu not exist"));
    }

    @Override
    public List<MenuEntityVo> tree() {
        List<MenuEntity> menus =  menuRepository.findAllByOrderByOrderNumDesc();

        List<MenuEntityVo> menuEntityVos = menus.
                stream().
                map(menu ->
                        MenuEntityVo.
                        builder().
                        menuId(menu.
                                getMenuId()).
                        parentId(menu.
                                getParentId()).
                        icon(menu.
                                getIcon()).
                        url(menu.
                                getUrl()).
                        title(menu.
                                getTitle()).
                        name(menu.
                                getName()).
                        component(menu.
                                getComponent()).
                        type(menu.
                                getType()).
                        orderNum(menu.
                                getOrderNum()).
                        status(menu.
                                getStatus()).
                        build())
                .toList();

        return buildTreeMenu(menuEntityVos);
    }

    @Override
    public void saveOrUpdate(MenuEntityVo menu) {
        MenuEntity menuEntity = MenuEntity.
                builder().
                menuId(menu.
                        getMenuId()).
                parentId(menu.
                        getParentId()).
                icon(menu.
                        getIcon()).
                url(menu.
                        getUrl()).
                title(menu.
                        getTitle()).
                name(menu.
                        getName()).
                component(menu.
                        getComponent()).
                type(menu.
                        getType()).
                orderNum(menu.
                        getOrderNum()).
                status(menu.
                        getStatus()).
                build();

        menuRepository.save(menuEntity);
    }

    @Override
    public void delete(Long id) {
        menuRepository.deleteById(id);
    }

    private List<MenuEntityVo> buildTreeMenu(List<MenuEntityVo> menus) {
        //2.组装父子的树形结构
        //2.1 找到所有一级分类
        return menus.
                stream().
                filter(menu -> menu.getParentId() == 0).
                peek(menu-> menu.setChildren(getChildren(menu, menus))).
                sorted(Comparator.comparingInt(menu -> Optional.ofNullable(menu.getOrderNum()).isEmpty() ? 0 : menu.getOrderNum())).
                toList();
    }

    private List<MenuEntityVo> getChildren(MenuEntityVo root, List<MenuEntityVo> all) {
        return all.
                stream().
                filter(menu -> Objects.equals(menu.getParentId(), root.getMenuId())).
                peek(menu -> menu.setChildren(getChildren(menu, all))).
                sorted(Comparator.comparingInt(menu -> Optional.ofNullable(menu.getOrderNum()).isEmpty() ? 0 : menu.getOrderNum())).
                toList();
    }
}
