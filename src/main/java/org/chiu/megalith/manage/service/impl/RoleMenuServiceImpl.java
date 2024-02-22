package org.chiu.megalith.manage.service.impl;

import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.manage.convertor.MenuVoConvertor;
import org.chiu.megalith.manage.entity.*;
import org.chiu.megalith.manage.repository.*;
import org.chiu.megalith.manage.service.RoleMenuService;
import org.chiu.megalith.manage.vo.RoleAuthorityVo;
import org.chiu.megalith.manage.vo.RoleMenuVo;
import org.chiu.megalith.manage.vo.MenuVo;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

import static org.chiu.megalith.infra.lang.ExceptionMessage.NO_FOUND;
import static org.chiu.megalith.infra.lang.ExceptionMessage.ROLE_NOT_EXIST;
import static org.chiu.megalith.infra.lang.StatusEnum.NORMAL;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:26 am
 */
@Service
@RequiredArgsConstructor
public class RoleMenuServiceImpl implements RoleMenuService {

    private final RoleRepository roleRepository;

    private final UserRepository userRepository;

    private final MenuRepository menuRepository;

    private final RoleMenuRepository roleMenuRepository;

    private final AuthorityRepository authorityRepository;

    private final RoleAuthorityRepository roleAuthorityRepository;

    private List<RoleMenuVo> setCheckMenusInfo(List<MenuVo> menusInfo, List<Long> menuIdsByRole, RoleMenuVo.RoleMenuVoBuilder parent, List<RoleMenuVo> parentChildren) {
        menusInfo.forEach(item -> {
            RoleMenuVo.RoleMenuVoBuilder builder = RoleMenuVo.builder()
                    .title(item.getTitle())
                    .menuId(item.getMenuId());

            if (menuIdsByRole.contains(item.getMenuId())) {
                builder.check(true);
            }

            if (Boolean.FALSE.equals(item.getChildren().isEmpty())) {
                List<RoleMenuVo> children = new ArrayList<>();
                builder.children(children);
                setCheckMenusInfo(item.getChildren(), menuIdsByRole, builder, children);
            }
            parentChildren.add(builder.build());
        });

        return parentChildren;
    }

    @Override
    public List<Long> getNavMenuIdsByRoleId(String role) {
        RoleEntity roleEntity = roleRepository.findByCodeAndStatus(role, NORMAL.getCode())
                .orElseThrow(() -> new MissException(ROLE_NOT_EXIST));
        Long id = roleEntity.getId();
        return roleMenuRepository.findMenuIdsByRoleId(id);
    }

    @Override
    public List<MenuVo> getCurrentUserNav(Long userId) {
        UserEntity userEntity = userRepository.findById(userId)
                .orElseThrow(() -> new MissException(NO_FOUND));
        String role = userEntity.getRole();
        List<Long> menuIds = getNavMenuIdsByRoleId(role);
        return buildMenu(menuIds, true);
    }

    private List<MenuVo> buildMenu(List<Long> menuIds, Boolean statusCheck) {
        List<MenuEntity> menus = menuRepository.findAllById(menuIds);
        List<MenuVo> menuEntities = MenuVoConvertor.convert(menus, statusCheck);
        // 转树状结构
        return buildTreeMenu(menuEntities);
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

    public List<RoleMenuVo> getMenusInfo(Long roleId) {
        List<MenuVo> menusInfo = getNormalMenusInfo();
        List<Long> menuIdsByRole = roleMenuRepository.findMenuIdsByRoleId(roleId);
        return setCheckMenusInfo(menusInfo, menuIdsByRole, null, new ArrayList<>());
    }

    @Override
    public List<MenuVo> tree() {
        List<MenuEntity> menus =  menuRepository.findAllByOrderByOrderNumDesc();
        List<MenuVo> menuEntities = MenuVoConvertor.convert(menus);
        return buildTreeMenu(menuEntities);
    }

    @Override
    public List<RoleAuthorityVo> getAuthoritiesInfo(Long roleId) {
        List<AuthorityEntity> allAuthorityEntities = authorityRepository.findByStatus(NORMAL.getCode());
        List<RoleAuthorityEntity> authorityEntities = roleAuthorityRepository.findByRoleId(roleId);

        List<Long> ids = authorityEntities.stream()
                .map(RoleAuthorityEntity::getAuthorityId)
                .toList();
        List<RoleAuthorityVo> roleAuthorityVos = new ArrayList<>();

        allAuthorityEntities.forEach(item -> roleAuthorityVos
                .add(RoleAuthorityVo.builder()
                        .authorityId(item.getId())
                        .code(item.getCode())
                        .check(ids.contains(item.getId()))
                        .build()));
        return roleAuthorityVos;
    }

    public List<MenuVo> getNormalMenusInfo() {
        List<Long> menuIds = menuRepository.findAllIds();
        return buildMenu(menuIds, true);
    }
}
