package org.chiu.megalith.manage.service.impl;

import org.chiu.megalith.manage.convertor.MenuDisplayVoConvertor;
import org.chiu.megalith.manage.convertor.RoleMenuEntityConvertor;
import org.chiu.megalith.manage.entity.*;
import org.chiu.megalith.manage.repository.*;
import org.chiu.megalith.manage.service.RoleMenuService;
import org.chiu.megalith.manage.vo.MenuDisplayVo;
import org.chiu.megalith.manage.vo.RoleMenuVo;
import org.chiu.megalith.manage.vo.MenuVo;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.wrapper.RoleMenuWrapper;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

import static org.chiu.megalith.infra.lang.Const.ROLE_PREFIX;
import static org.chiu.megalith.manage.convertor.MenuDisplayVoConvertor.buildTreeMenu;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:26 am
 */
@Service
@RequiredArgsConstructor
public class RoleMenuServiceImpl implements RoleMenuService {

    private final MenuRepository menuRepository;

    private final RoleMenuRepository roleMenuRepository;

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
        return roleMenuWrapper.getCurrentRoleNav(role);
    }

    public List<RoleMenuVo> getMenusInfo(Long roleId) {
        List<MenuDisplayVo> menusInfo = getNormalMenusInfo();
        List<Long> menuIdsByRole = roleMenuRepository.findMenuIdsByRoleId(roleId);
        return setCheckMenusInfo(menusInfo, menuIdsByRole, null, new ArrayList<>());
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
        return buildTreeMenu(menuEntities);
    }
}
