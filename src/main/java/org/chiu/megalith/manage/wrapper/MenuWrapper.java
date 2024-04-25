package org.chiu.megalith.manage.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.cache.CacheEvict;
import org.chiu.megalith.manage.cache.handler.AllMenuAndButtonCacheEvictHandler;
import org.chiu.megalith.manage.entity.MenuEntity;
import org.chiu.megalith.manage.repository.MenuRepository;
import org.chiu.megalith.manage.repository.RoleMenuRepository;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;

@Component
@RequiredArgsConstructor
public class MenuWrapper {

    private final MenuRepository menuRepository;

    private final RoleMenuRepository roleMenuRepository;

    @Transactional
    @CacheEvict(handler = { AllMenuAndButtonCacheEvictHandler.class })
    public void deleteMenu(Long id) {
        menuRepository.deleteById(id);
        roleMenuRepository.deleteByMenuId(id);
    }

    @CacheEvict(handler = { AllMenuAndButtonCacheEvictHandler.class })
    public void save(MenuEntity menuEntity) {
        menuRepository.save(menuEntity);
    }

    @CacheEvict(handler = { AllMenuAndButtonCacheEvictHandler.class })
    public void saveAll(ArrayList<MenuEntity> menuEntities) {
        menuRepository.saveAll(menuEntities);
    }
}
