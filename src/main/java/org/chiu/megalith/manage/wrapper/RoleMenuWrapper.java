package org.chiu.megalith.manage.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.cache.Cache;
import org.chiu.megalith.manage.cache.CacheEvict;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.manage.cache.handler.MenuAndButtonCacheEvictHandler;
import org.chiu.megalith.manage.convertor.ButtonVoConvertor;
import org.chiu.megalith.manage.convertor.MenuDisplayVoConvertor;
import org.chiu.megalith.manage.convertor.MenuVoConvertor;
import org.chiu.megalith.manage.entity.MenuEntity;
import org.chiu.megalith.manage.entity.RoleEntity;
import org.chiu.megalith.manage.entity.RoleMenuEntity;
import org.chiu.megalith.manage.repository.MenuRepository;
import org.chiu.megalith.manage.repository.RoleMenuRepository;
import org.chiu.megalith.manage.repository.RoleRepository;
import org.chiu.megalith.manage.vo.ButtonVo;
import org.chiu.megalith.manage.vo.MenuDisplayVo;
import org.chiu.megalith.manage.vo.MenuVo;
import org.chiu.megalith.manage.vo.MenusAndButtons;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

import static org.chiu.megalith.infra.lang.ExceptionMessage.ROLE_NOT_EXIST;
import static org.chiu.megalith.infra.lang.StatusEnum.NORMAL;
import static org.chiu.megalith.infra.lang.TypeMenu.*;
import static org.chiu.megalith.manage.convertor.MenuDisplayVoConvertor.buildTreeMenu;

@Component
@RequiredArgsConstructor
public class RoleMenuWrapper {

    private final RoleMenuRepository roleMenuRepository;

    private final RoleRepository roleRepository;

    private final MenuRepository menuRepository;


    @Transactional
    @CacheEvict(handler = { MenuAndButtonCacheEvictHandler.class })
    public void saveMenu(Long roleId, ArrayList<RoleMenuEntity> roleMenuEntities) {
        roleMenuRepository.deleteByRoleId(roleId);
        roleMenuRepository.saveAll(roleMenuEntities);
    }

    @Cache(prefix = Const.HOT_MENUS_AND_BUTTONS)
    public MenusAndButtons getCurrentRoleNav(String role) {
        RoleEntity roleEntity = roleRepository.findByCodeAndStatus(role, NORMAL.getCode())
                .orElseThrow(() -> new MissException(ROLE_NOT_EXIST));
        Long id = roleEntity.getId();
        List<Long> menuIds = roleMenuRepository.findMenuIdsByRoleId(id);
        List<MenuEntity> allKindsInfo = menuRepository.findAllById(menuIds);

        List<MenuEntity> menus = allKindsInfo
                .stream()
                .filter(menu -> CATALOGUE.getCode().equals(menu.getType()) || MENU.getCode().equals(menu.getType()))
                .toList();

        List<MenuEntity> buttons = allKindsInfo
                .stream()
                .filter(menu -> BUTTON.getCode().equals(menu.getType()))
                .toList();

        List<MenuDisplayVo> menuEntities = MenuDisplayVoConvertor.convert(menus, true);
        List<MenuDisplayVo> displayVos = buildTreeMenu(menuEntities);
        List<MenuVo> menuVos = MenuVoConvertor.convert(displayVos);
        List<ButtonVo> buttonVos = ButtonVoConvertor.convert(buttons, true);
        return MenusAndButtons.builder()
                .buttons(buttonVos)
                .menus(menuVos)
                .build();
    }
}
