package org.chiu.megalith.authority.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.authority.convertor.ButtonDtoConvertor;
import org.chiu.megalith.authority.convertor.MenuDisplayDtoConvertor;
import org.chiu.megalith.authority.convertor.MenuDtoConvertor;
import org.chiu.megalith.infra.cache.Cache;
import org.chiu.megalith.authority.cache.CacheEvict;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.authority.cache.handler.MenuAndButtonCacheEvictHandler;
import org.chiu.megalith.authority.dto.ButtonDto;
import org.chiu.megalith.authority.dto.MenuDisplayDto;
import org.chiu.megalith.authority.dto.MenuDto;
import org.chiu.megalith.authority.dto.MenusAndButtonsDto;
import org.chiu.megalith.authority.entity.MenuEntity;
import org.chiu.megalith.authority.entity.RoleEntity;
import org.chiu.megalith.authority.entity.RoleMenuEntity;
import org.chiu.megalith.authority.repository.MenuRepository;
import org.chiu.megalith.authority.repository.RoleMenuRepository;
import org.chiu.megalith.authority.repository.RoleRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

import static org.chiu.megalith.infra.lang.ExceptionMessage.ROLE_NOT_EXIST;
import static org.chiu.megalith.infra.lang.StatusEnum.NORMAL;
import static org.chiu.megalith.infra.lang.TypeMenu.*;

@Component
@RequiredArgsConstructor
public class RoleMenuWrapper {

    private final RoleMenuRepository roleMenuRepository;

    private final RoleRepository roleRepository;

    private final MenuRepository menuRepository;


    @Transactional
    @CacheEvict(handler = { MenuAndButtonCacheEvictHandler.class })
    public void saveMenu(Long roleId, List<RoleMenuEntity> roleMenuEntities) {
        roleMenuRepository.deleteByRoleId(roleId);
        roleMenuRepository.saveAll(roleMenuEntities);
    }

    @Cache(prefix = Const.HOT_MENUS_AND_BUTTONS)
    public MenusAndButtonsDto getCurrentRoleNav(String role) {
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

        List<MenuDisplayDto> menuEntities = MenuDisplayDtoConvertor.convert(menus, true);
        List<MenuDisplayDto> displayDtos = MenuDisplayDtoConvertor.buildTreeMenu(menuEntities);
        List<MenuDto> menuDtos = MenuDtoConvertor.convert(displayDtos);
        List<ButtonDto> buttonDtos = ButtonDtoConvertor.convert(buttons, true);
        return MenusAndButtonsDto.builder()
                .buttons(buttonDtos)
                .menus(menuDtos)
                .build();
    }
}
