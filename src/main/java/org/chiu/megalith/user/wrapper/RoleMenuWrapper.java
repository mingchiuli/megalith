package org.chiu.megalith.user.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.user.cache.handler.AllMenuAndButtonCacheEvictHandler;
import org.chiu.megalith.user.convertor.ButtonDtoConvertor;
import org.chiu.megalith.user.convertor.MenuDisplayDtoConvertor;
import org.chiu.megalith.user.convertor.MenuDtoConvertor;
import org.chiu.megalith.infra.cache.Cache;
import org.chiu.megalith.user.cache.CacheEvict;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.user.cache.handler.MenuAndButtonCacheEvictHandler;
import org.chiu.megalith.user.dto.ButtonDto;
import org.chiu.megalith.user.dto.MenuDisplayDto;
import org.chiu.megalith.user.dto.MenuDto;
import org.chiu.megalith.user.dto.MenusAndButtonsDto;
import org.chiu.megalith.user.entity.MenuEntity;
import org.chiu.megalith.user.entity.RoleEntity;
import org.chiu.megalith.user.entity.RoleMenuEntity;
import org.chiu.megalith.user.repository.MenuRepository;
import org.chiu.megalith.user.repository.RoleMenuRepository;
import org.chiu.megalith.user.repository.RoleRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

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
        Optional<RoleEntity> roleEntityOptional = roleRepository.findByCodeAndStatus(role, NORMAL.getCode());

        if (roleEntityOptional.isEmpty()) {
            return MenusAndButtonsDto.builder()
                    .menus(Collections.emptyList())
                    .buttons(Collections.emptyList())
                    .build();
        }

        RoleEntity roleEntity = roleEntityOptional.get();

        List<Long> menuIds = roleMenuRepository.findMenuIdsByRoleId(roleEntity.getId()).stream()
                .distinct()
                .toList();

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

    @Transactional
    @CacheEvict(handler = { AllMenuAndButtonCacheEvictHandler.class })
    public void deleteMenu(Long id) {
        menuRepository.deleteById(id);
        roleMenuRepository.deleteByMenuId(id);
    }
}
