package org.chiu.megalith.authority.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.authority.cache.CacheEvict;
import org.chiu.megalith.authority.cache.handler.AllMenuAndButtonCacheEvictHandler;
import org.chiu.megalith.authority.entity.MenuEntity;
import org.chiu.megalith.authority.repository.MenuRepository;
import org.chiu.megalith.authority.repository.RoleMenuRepository;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

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
    public void saveAll(List<MenuEntity> menuEntities) {
        menuRepository.saveAll(menuEntities);
    }
}
