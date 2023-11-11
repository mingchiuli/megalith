package org.chiu.megalith.manage.service.impl;

import org.chiu.megalith.manage.entity.RoleMenuEntity;
import org.chiu.megalith.manage.repository.RoleMenuRepository;
import org.chiu.megalith.manage.service.RoleMenuService;
import org.chiu.megalith.manage.vo.MenuRoleVo;
import org.chiu.megalith.manage.vo.MenuVo;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:26 am
 */
@Service
@RequiredArgsConstructor
public class RoleMenuServiceImpl implements RoleMenuService {

    private final RoleMenuRepository roleMenuRepository;

    @Override
    public void deleteByRoleId(Long roleId) {
        roleMenuRepository.deleteByRoleId(roleId);
    }

    @Override
    public void deleteByMenuId(Long menuId) {
        roleMenuRepository.deleteByMenuId(menuId);
    }

    @Override
    public List<Long> findMenuIdsByRoleId(Long id) {
        return roleMenuRepository.findMenuIdsByRoleId(id);
    }

    @Override
    public void saveAll(List<RoleMenuEntity> roleMenus) {
        roleMenuRepository.saveAll(roleMenus);
    }

    @Override
    public List<MenuRoleVo> setCheckMenusInfo(List<MenuVo> menusInfo, List<Long> menuIdsByRole, MenuRoleVo.MenuRoleVoBuilder parent, List<MenuRoleVo> parentChildren) {
        menusInfo.forEach(item -> {
            MenuRoleVo.MenuRoleVoBuilder builder = MenuRoleVo.builder()
                    .title(item.getTitle())
                    .menuId(item.getMenuId());

            if (menuIdsByRole.contains(item.getMenuId())) {
                builder.check(true);
            }

            if (Boolean.FALSE.equals(item.getChildren().isEmpty())) {
                List<MenuRoleVo> children = new ArrayList<>();
                builder.children(children);
                setCheckMenusInfo(item.getChildren(), menuIdsByRole, builder, children);
            }
            parentChildren.add(builder.build());
        });

        return parentChildren;
    }
}
