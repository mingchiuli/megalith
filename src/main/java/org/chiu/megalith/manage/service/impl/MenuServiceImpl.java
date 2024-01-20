package org.chiu.megalith.manage.service.impl;

import org.chiu.megalith.infra.exception.CommitException;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.manage.convertor.MenuEntityConvertor;
import org.chiu.megalith.manage.convertor.MenuEntityVoConvertor;
import org.chiu.megalith.manage.entity.MenuEntity;
import org.chiu.megalith.manage.repository.MenuRepository;
import org.chiu.megalith.manage.service.MenuService;
import org.chiu.megalith.manage.req.MenuEntityReq;
import org.chiu.megalith.infra.exception.MissException;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.vo.MenuEntityVo;
import org.chiu.megalith.manage.wrapper.MenuWrapper;
import org.springframework.stereotype.Service;

import java.util.*;

import static org.chiu.megalith.infra.lang.ExceptionMessage.MENU_INVALID_OPERATE;
import static org.chiu.megalith.infra.lang.ExceptionMessage.MENU_NOT_EXIST;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
@Service
@RequiredArgsConstructor
public class MenuServiceImpl implements MenuService {

    private final MenuRepository menuRepository;

    private final MenuWrapper menuWrapper;

    @Override
    public MenuEntityVo findById(Long id) {
        MenuEntity menuEntity = menuRepository.findById(id)
                .orElseThrow(() -> new MissException(MENU_NOT_EXIST.getMsg()));

        return MenuEntityVoConvertor.convert(menuEntity);
    }

    @Override
    public void saveOrUpdate(MenuEntityReq menu) {

        MenuEntity menuEntity = MenuEntityConvertor.convert(menu);

        if (StatusEnum.HIDE.getCode().equals(menu.getStatus())) {
            List<MenuEntity> menuEntities = new ArrayList<>();
            menuEntities.add(menuEntity);
            Long menuId = menu.getMenuId();
            findTargetChildrenMenuId(menuId, menuEntities);
            menuRepository.saveAll(menuEntities);
            return;
        }
        menuRepository.save(menuEntity);
    }

    private void findTargetChildrenMenuId(Long menuId, List<MenuEntity> menuEntities) {
        List<MenuEntity> menus = menuRepository.findByParentId(menuId);
        menus.forEach(menu -> {
            menu.setStatus(StatusEnum.HIDE.getCode());
            menuEntities.add(menu);
            findTargetChildrenMenuId(menu.getMenuId(), menuEntities);
        });
    }

    @Override
    public void delete(Long id) {
        List<MenuEntity> menus = menuRepository.findByParentId(id);
        if (Boolean.FALSE.equals(menus.isEmpty())) {
            throw new CommitException(MENU_INVALID_OPERATE);
        }
        menuWrapper.deleteMenu(id);
    }

}
