package org.chiu.megalith.manage.service.impl;

import org.chiu.megalith.infra.exception.CommitException;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.service.RoleMenuService;
import org.chiu.megalith.manage.service.UserService;
import org.chiu.megalith.manage.entity.MenuEntity;
import org.chiu.megalith.manage.repository.MenuRepository;
import org.chiu.megalith.manage.service.MenuService;
import org.chiu.megalith.manage.service.RoleService;
import org.chiu.megalith.manage.req.MenuEntityReq;
import org.chiu.megalith.infra.exception.MissException;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.vo.MenuEntityVo;
import org.chiu.megalith.manage.vo.MenuVo;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Stream;

import static org.chiu.megalith.infra.lang.ExceptionMessage.MENU_INVALID_OPERATE;
import static org.chiu.megalith.infra.lang.ExceptionMessage.MENU_NOT_EXIST;

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

    private final RoleMenuService roleMenuService;

    @Override
    public List<MenuVo> getCurrentUserNav(Long userId) {

        UserEntity userEntity = userService.findById(userId);
        String role = userEntity.getRole();
        List<Long> menuIds = roleService.getNavMenuIdsByRoleId(role);
        return buildMenu(menuIds, true);
    }

    @Override
    public List<MenuVo> getNormalMenusInfo() {
        List<Long> menuIds = menuRepository.findAllIds();
        return buildMenu(menuIds, true);
    }

    @Override
    public List<MenuVo> buildMenu(List<Long> menuIds, Boolean statusCheck) {
        List<MenuEntity> menus = menuRepository.findAllById(menuIds);
        Stream<MenuEntity> menuStream = menus.stream();
        if (Boolean.TRUE.equals(statusCheck)) {
            menuStream = menuStream.filter(menu -> StatusEnum.NORMAL.getCode().equals(menu.getStatus()));
        }

        List<MenuVo> menuEntities = menuStream
                .map(menu -> MenuVo.builder()
                        .menuId(menu.getMenuId())
                        .parentId(menu.getParentId())
                        .icon(menu.getIcon())
                        .url(menu.getUrl())
                        .title(menu.getTitle())
                        .name(menu.getName())
                        .component(menu.getComponent())
                        .type(menu.getType())
                        .orderNum(menu.getOrderNum())
                        .status(menu.getStatus())
                        .build())
                .toList();
        // 转树状结构
        return buildTreeMenu(menuEntities);
    }

    @Override
    public MenuEntityVo findById(Long id) {
        MenuEntity menuEntity = menuRepository.findById(id)
                .orElseThrow(() -> new MissException(MENU_NOT_EXIST.getMsg()));

        return MenuEntityVo.builder()
                .menuId(menuEntity.getMenuId())
                .url(menuEntity.getUrl())
                .title(menuEntity.getTitle())
                .type(menuEntity.getType())
                .name(menuEntity.getName())
                .component(menuEntity.getComponent())
                .orderNum(menuEntity.getOrderNum())
                .parentId(menuEntity.getParentId())
                .icon(menuEntity.getIcon())
                .status(menuEntity.getStatus())
                .build();
    }

    @Override
    public List<MenuVo> tree() {

        List<MenuEntity> menus =  menuRepository.findAllByOrderByOrderNumDesc();
        List<MenuVo> menuEntities = menus.stream()
                .map(menu -> MenuVo.builder()
                        .menuId(menu.getMenuId())
                        .parentId(menu.getParentId())
                        .icon(menu.getIcon())
                        .url(menu.getUrl())
                        .title(menu.getTitle())
                        .name(menu.getName())
                        .component(menu.getComponent())
                        .type(menu.getType())
                        .orderNum(menu.getOrderNum())
                        .status(menu.getStatus())
                        .build())
                .toList();

        return buildTreeMenu(menuEntities);
    }

    @Override
    public void saveOrUpdate(MenuEntityReq menu) {

        var menuEntity = MenuEntity.builder()
                .menuId(menu.getMenuId())
                .parentId(menu.getParentId())
                .icon(menu.getIcon())
                .url(menu.getUrl())
                .title(menu.getTitle())
                .name(menu.getName())
                .component(menu.getComponent())
                .type(menu.getType())
                .orderNum(menu.getOrderNum())
                .status(menu.getStatus())
                .build();

        menuRepository.save(menuEntity);
    }

    @Override
    @Transactional
    public void delete(Long id) {
        List<MenuEntity> menus = menuRepository.findByParentId(id);
        if (Boolean.FALSE.equals(menus.isEmpty())) {
            throw new CommitException(MENU_INVALID_OPERATE);
        }

        menuRepository.deleteById(id);
        roleMenuService.deleteByMenuId(id);
    }

    private List<MenuVo> buildTreeMenu(List<MenuVo> menus) {
        //2.组装父子的树形结构
        //2.1 找到所有一级分类
        return menus.stream()
                .filter(menu -> menu.getParentId() == 0)
                .peek(menu-> menu.setChildren(getChildren(menu, menus)))
                .sorted(Comparator.comparingInt(menu -> Objects.isNull(menu.getOrderNum()) ? 0 : menu.getOrderNum()))
                .toList();
    }

    private List<MenuVo> getChildren(MenuVo root, List<MenuVo> all) {
        return all.stream()
                .filter(menu -> Objects.equals(menu.getParentId(), root.getMenuId()))
                .peek(menu -> menu.setChildren(getChildren(menu, all)))
                .sorted(Comparator.comparingInt(menu -> Objects.isNull(menu.getOrderNum()) ? 0 : menu.getOrderNum()))
                .toList();
    }
}
