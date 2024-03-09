package org.chiu.megalith.manage.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.cache.Cache;
import org.chiu.megalith.infra.cache.CacheEvict;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.manage.convertor.MenuDisplayVoConvertor;
import org.chiu.megalith.manage.convertor.MenuVoConvertor;
import org.chiu.megalith.manage.entity.MenuEntity;
import org.chiu.megalith.manage.entity.RoleEntity;
import org.chiu.megalith.manage.repository.MenuRepository;
import org.chiu.megalith.manage.repository.RoleMenuRepository;
import org.chiu.megalith.manage.repository.RoleRepository;
import org.chiu.megalith.manage.utils.MenuUtils;
import org.chiu.megalith.manage.vo.MenuDisplayVo;
import org.chiu.megalith.manage.vo.MenuVo;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

import static org.chiu.megalith.infra.lang.ExceptionMessage.ROLE_NOT_EXIST;
import static org.chiu.megalith.infra.lang.StatusEnum.NORMAL;

@Component
@RequiredArgsConstructor
public class MenuWrapper {

    private final MenuRepository menuRepository;

    private final RoleMenuRepository roleMenuRepository;

    private final RoleRepository roleRepository;

    @Transactional
    @CacheEvict(prefix = {Const.HOT_MENUS})
    public void deleteMenu(Long id) {
        menuRepository.deleteById(id);
        roleMenuRepository.deleteByMenuId(id);
    }

    @Cache(prefix = Const.HOT_MENUS)
    public List<MenuVo> getCurrentRoleNav(String role) {
        RoleEntity roleEntity = roleRepository.findByCodeAndStatus(role, NORMAL.getCode())
                .orElseThrow(() -> new MissException(ROLE_NOT_EXIST));
        Long id = roleEntity.getId();
        List<Long> menuIds = roleMenuRepository.findMenuIdsByRoleId(id);
        List<MenuEntity> menus = menuRepository.findAllById(menuIds);
        List<MenuDisplayVo> menuEntities = MenuDisplayVoConvertor.convert(menus, true);
        List<MenuDisplayVo> displayVos = MenuUtils.buildTreeMenu(menuEntities);
        return MenuVoConvertor.convert(displayVos);
    }

    @CacheEvict(prefix = {Const.HOT_MENUS})
    public void save(MenuEntity menuEntity) {
        menuRepository.save(menuEntity);
    }
}
