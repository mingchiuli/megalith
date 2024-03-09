package org.chiu.megalith.manage.service.impl;

import org.chiu.megalith.manage.convertor.MenuDisplayVoConvertor;
import org.chiu.megalith.manage.convertor.RoleAuthorityEntityConvertor;
import org.chiu.megalith.manage.convertor.RoleMenuEntityConvertor;
import org.chiu.megalith.manage.entity.*;
import org.chiu.megalith.manage.repository.*;
import org.chiu.megalith.manage.service.RoleMenuService;
import org.chiu.megalith.manage.utils.MenuUtils;
import org.chiu.megalith.manage.vo.MenuDisplayVo;
import org.chiu.megalith.manage.vo.RoleAuthorityVo;
import org.chiu.megalith.manage.vo.RoleMenuVo;
import org.chiu.megalith.manage.vo.MenuVo;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.wrapper.MenuWrapper;
import org.chiu.megalith.manage.wrapper.RoleAuthorityWrapper;
import org.chiu.megalith.manage.wrapper.RoleMenuWrapper;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

import static org.chiu.megalith.infra.lang.Const.ROLE_PREFIX;
import static org.chiu.megalith.infra.lang.StatusEnum.NORMAL;
import static org.chiu.megalith.manage.utils.MenuUtils.buildTreeMenu;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:26 am
 */
@Service
@RequiredArgsConstructor
public class RoleMenuServiceImpl implements RoleMenuService {

    private final MenuRepository menuRepository;

    private final RoleMenuRepository roleMenuRepository;

    private final AuthorityRepository authorityRepository;

    private final RoleAuthorityRepository roleAuthorityRepository;

    private final RoleAuthorityWrapper roleAuthorityWrapper;

    private final MenuWrapper menuWrapper;

    private final RoleMenuWrapper roleMenuWrapper;

    private List<RoleMenuVo> setCheckMenusInfo(List<MenuDisplayVo> menusInfo, List<Long> menuIdsByRole, RoleMenuVo.RoleMenuVoBuilder parent, List<RoleMenuVo> parentChildren) {
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
    public List<MenuVo> getCurrentUserNav(String role) {
        role = role.substring(ROLE_PREFIX.getInfo().length());
        return menuWrapper.getCurrentRoleNav(role);
    }

    public List<RoleMenuVo> getMenusInfo(Long roleId) {
        List<MenuDisplayVo> menusInfo = getNormalMenusInfo();
        List<Long> menuIdsByRole = roleMenuRepository.findMenuIdsByRoleId(roleId);
        return setCheckMenusInfo(menusInfo, menuIdsByRole, null, new ArrayList<>());
    }

    @Override
    public List<MenuDisplayVo> tree() {
        List<MenuEntity> menus =  menuRepository.findAllByOrderByOrderNumDesc();
        List<MenuDisplayVo> menuEntities = MenuDisplayVoConvertor.convert(menus, false);
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

    /**
     * 找不到方法的问题，只能用ArrayList
     * @param roleId
     * @param authorityIds
     */
    public void saveAuthority(Long roleId, ArrayList<Long> authorityIds) {
        List<RoleAuthorityEntity> roleAuthorityEntities = RoleAuthorityEntityConvertor.convert(roleId, authorityIds);
        roleAuthorityWrapper.saveAuthority(roleId, new ArrayList<>(roleAuthorityEntities));
    }

    @Override
    public void saveMenu(Long roleId, ArrayList<Long> menuIds) {
        List<RoleMenuEntity> roleMenuEntities = RoleMenuEntityConvertor.convert(roleId, menuIds);
        roleMenuWrapper.saveMenu(roleId, new ArrayList<>(roleMenuEntities));
    }

    public List<MenuDisplayVo> getNormalMenusInfo() {
        List<Long> menuIds = menuRepository.findAllIds();
        return buildMenu(menuIds, true);
    }

    private List<MenuDisplayVo> buildMenu(List<Long> menuIds, Boolean statusCheck) {
        List<MenuEntity> menus = menuRepository.findAllById(menuIds);
        List<MenuDisplayVo> menuEntities = MenuDisplayVoConvertor.convert(menus, statusCheck);
        // 转树状结构
        return MenuUtils.buildTreeMenu(menuEntities);
    }
}
