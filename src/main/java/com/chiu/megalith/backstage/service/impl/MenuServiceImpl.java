package com.chiu.megalith.backstage.service.impl;

import com.chiu.megalith.authentication.user.entity.UserEntity;
import com.chiu.megalith.authentication.user.service.UserService;
import com.chiu.megalith.backstage.entity.MenuEntity;
import com.chiu.megalith.backstage.repository.MenuRepository;
import com.chiu.megalith.backstage.service.MenuService;
import com.chiu.megalith.backstage.service.RoleService;
import com.chiu.megalith.backstage.vo.MenuEntityVo;
import com.chiu.megalith.common.exception.NotFoundException;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

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
    public List<MenuEntityVo> getCurrentUserNav(String username) {

        UserEntity userEntity = userService.retrieveUserInfo(username);
        String role = userEntity.getRole();

        List<Long> menuIds = roleService.getNavMenuIds(role);
        Iterable<MenuEntity> menus = menuRepository.findAllById(menuIds);
        ArrayList<MenuEntityVo> entities = new ArrayList<>();
        menus.forEach(menu -> {
            MenuEntityVo entity = new MenuEntityVo();
            BeanUtils.copyProperties(menu, entity);
            entities.add(entity);
        });
        // 转树状结构
        return buildTreeMenu(entities);

    }

    @Override
    public MenuEntity findById(Long id) {
        return menuRepository.findById(id).orElseThrow(() -> new NotFoundException("menu not exist"));
    }

    @Override
    public List<MenuEntityVo> tree() {
        List<MenuEntity> menus =  menuRepository.findAllByOrderByOrderNumDesc();
        ArrayList<MenuEntityVo> entityVos = new ArrayList<>();
        menus.forEach(menu -> {
            MenuEntityVo menuEntityVo = new MenuEntityVo();
            BeanUtils.copyProperties(menu, menuEntityVo);
            entityVos.add(menuEntityVo);
        });
        return buildTreeMenu(entityVos);
    }

    @Override
    public void saveOrUpdate(MenuEntityVo menu) {
        MenuEntity menuEntity = new MenuEntity();
        BeanUtils.copyProperties(menu, menuEntity);
        menuRepository.save(menuEntity);
    }

    @Override
    public void delete(Long id) {
        menuRepository.deleteById(id);
    }

    private List<MenuEntityVo> buildTreeMenu(List<MenuEntityVo> menus) {
        //2.组装父子的树形结构
        //2.1 找到所有一级分类
        return menus.stream()
                .filter(menu -> menu.getParentId() == 0)
                .peek(menu-> menu.setChildren(getChildren(menu, menus)))
                .sorted(Comparator.comparingInt(menu -> (menu.getOrderNum() == null ? 0 : menu.getOrderNum())))
                .collect(Collectors.toList());
    }

    private List<MenuEntityVo> getChildren(MenuEntityVo root, List<MenuEntityVo> all) {
        return all.stream()
                .filter(menu -> Objects.equals(menu.getParentId(), root.getMenuId()))
                .peek(menu -> menu.setChildren(getChildren(menu, all)))
                .sorted(Comparator.comparingInt(menu -> (menu.getOrderNum() == null ? 0 : menu.getOrderNum())))
                .collect(Collectors.toList());
    }
}
