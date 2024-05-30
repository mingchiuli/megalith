package org.chiu.megalith.user.service.impl;

import org.chiu.megalith.infra.exception.CommitException;
import org.chiu.megalith.user.convertor.MenuDisplayVoConvertor;
import org.chiu.megalith.user.convertor.MenusAndButtonsVoConvertor;
import org.chiu.megalith.user.convertor.RoleMenuEntityConvertor;
import org.chiu.megalith.user.dto.ButtonDto;
import org.chiu.megalith.user.dto.MenuDto;
import org.chiu.megalith.user.dto.MenusAndButtonsDto;
import org.chiu.megalith.user.entity.MenuEntity;
import org.chiu.megalith.user.entity.RoleMenuEntity;
import org.chiu.megalith.user.repository.MenuRepository;
import org.chiu.megalith.user.repository.RoleMenuRepository;
import org.chiu.megalith.user.service.RoleMenuService;
import org.chiu.megalith.user.vo.*;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.user.wrapper.RoleMenuWrapper;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

import static org.chiu.megalith.infra.lang.ExceptionMessage.MENU_INVALID_OPERATE;
import static org.chiu.megalith.user.convertor.MenuDisplayVoConvertor.buildTreeMenu;

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

    private List<RoleMenuVo> setCheckMenusInfo(List<MenuDisplayVo> menusInfo, List<Long> menuIdsByRole, List<RoleMenuVo> parentChildren) {
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
                setCheckMenusInfo(item.getChildren(), menuIdsByRole, children);
            }
            parentChildren.add(builder.build());
        });

        return parentChildren;
    }

    @Override
    public MenusAndButtonsVo getCurrentUserNav(List<String> roles) {
        List<MenuDto> allMenus = new ArrayList<>();
        List<ButtonDto> allButtons = new ArrayList<>();
        roles.forEach(role -> {
            MenusAndButtonsDto menusAndButtonsDto = roleMenuWrapper.getCurrentRoleNav(role);
            allMenus.addAll(menusAndButtonsDto.getMenus());
            allButtons.addAll(menusAndButtonsDto.getButtons());
        });

        return MenusAndButtonsVoConvertor.convertor(MenusAndButtonsDto.builder()
                .buttons(allButtons.stream()
                        .distinct()
                        .toList())
                .menus(allMenus.stream()
                        .distinct()
                        .toList())
                .build());
    }

    public List<RoleMenuVo> getMenusInfo(Long roleId) {
        List<MenuDisplayVo> menusInfo = getNormalMenusInfo();
        List<Long> menuIdsByRole = roleMenuRepository.findMenuIdsByRoleId(roleId);
        return setCheckMenusInfo(menusInfo, menuIdsByRole, new ArrayList<>());
    }

    @Override
    public void saveMenu(Long roleId, List<Long> menuIds) {
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

    @Override
    public void delete(Long id) {
        List<MenuEntity> menus = menuRepository.findByParentId(id);
        if (Boolean.FALSE.equals(menus.isEmpty())) {
            throw new CommitException(MENU_INVALID_OPERATE);
        }
        roleMenuWrapper.deleteMenu(id);
    }
}
