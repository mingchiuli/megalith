package org.chiu.megalith.manage.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.cache.CacheBatchEvict;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.manage.entity.MenuEntity;
import org.chiu.megalith.manage.repository.MenuRepository;
import org.chiu.megalith.manage.repository.RoleMenuRepository;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@RequiredArgsConstructor
public class MenuWrapper {

    private final MenuRepository menuRepository;

    private final RoleMenuRepository roleMenuRepository;

    @Transactional
    @CacheBatchEvict(prefix = {Const.HOT_MENUS_AND_BUTTONS})
    public void deleteMenu(Long id) {
        menuRepository.deleteById(id);
        roleMenuRepository.deleteByMenuId(id);
    }

    @CacheBatchEvict(prefix = {Const.HOT_MENUS_AND_BUTTONS})
    public void save(MenuEntity menuEntity) {
        menuRepository.save(menuEntity);
    }
}
