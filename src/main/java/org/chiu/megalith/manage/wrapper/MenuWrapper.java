package org.chiu.megalith.manage.wrapper;

import lombok.RequiredArgsConstructor;
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
    public void deleteMenu(Long id) {
        menuRepository.deleteById(id);
        roleMenuRepository.deleteByMenuId(id);
    }
}
