package org.chiu.megalith.user.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.user.cache.CacheEvict;
import org.chiu.megalith.user.cache.handler.AllMenuAndButtonCacheEvictHandler;
import org.chiu.megalith.user.entity.MenuEntity;
import org.chiu.megalith.user.repository.MenuRepository;

import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class MenuWrapper {

    private final MenuRepository menuRepository;


    @CacheEvict(handler = { AllMenuAndButtonCacheEvictHandler.class })
    public void save(MenuEntity menuEntity) {
        menuRepository.save(menuEntity);
    }

    @CacheEvict(handler = { AllMenuAndButtonCacheEvictHandler.class })
    public void saveAll(List<MenuEntity> menuEntities) {
        menuRepository.saveAll(menuEntities);
    }
}
