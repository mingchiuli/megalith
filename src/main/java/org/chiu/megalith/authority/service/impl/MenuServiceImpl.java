package org.chiu.megalith.authority.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import lombok.SneakyThrows;
import org.chiu.megalith.infra.exception.CommitException;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.authority.convertor.MenuDisplayVoConvertor;
import org.chiu.megalith.authority.convertor.MenuEntityConvertor;
import org.chiu.megalith.authority.convertor.MenuEntityVoConvertor;
import org.chiu.megalith.authority.entity.MenuEntity;
import org.chiu.megalith.authority.repository.MenuRepository;
import org.chiu.megalith.authority.service.MenuService;
import org.chiu.megalith.authority.req.MenuEntityReq;
import org.chiu.megalith.infra.exception.MissException;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.authority.vo.MenuDisplayVo;
import org.chiu.megalith.authority.vo.MenuEntityVo;
import org.chiu.megalith.authority.wrapper.MenuWrapper;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.*;

import static org.chiu.megalith.infra.lang.ExceptionMessage.*;
import static org.chiu.megalith.authority.convertor.MenuDisplayVoConvertor.buildTreeMenu;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
@Service
@RequiredArgsConstructor
public class MenuServiceImpl implements MenuService {

    private final MenuRepository menuRepository;

    private final MenuWrapper menuWrapper;

    private final ObjectMapper objectMapper;

    @Override
    public MenuEntityVo findById(Long id) {
        MenuEntity menuEntity = menuRepository.findById(id)
                .orElseThrow(() -> new MissException(MENU_NOT_EXIST.getMsg()));

        return MenuEntityVoConvertor.convert(menuEntity);
    }

    @Override
    public void saveOrUpdate(MenuEntityReq menu) {
        Long menuId = menu.getMenuId();
        MenuEntity menuEntity;

        if (Objects.nonNull(menuId)) {
            menuEntity = menuRepository.findById(menuId)
                     .orElseThrow(() -> new MissException(NO_FOUND));
        } else {
            menuEntity = MenuEntityConvertor.convert(menu);
        }

        BeanUtils.copyProperties(menu, menuEntity);
        
        if (StatusEnum.HIDE.getCode().equals(menu.getStatus())) {
            List<MenuEntity> menuEntities = new ArrayList<>();
            menuEntities.add(menuEntity);
            findTargetChildrenMenuId(menuId, menuEntities);
            menuWrapper.saveAll(menuEntities);
            return;
        }

        menuWrapper.save(menuEntity);
    }

    @Override
    public List<MenuDisplayVo> tree() {
        List<MenuEntity> menus =  menuRepository.findAllByOrderByOrderNumDesc();
        List<MenuDisplayVo> menuEntities = MenuDisplayVoConvertor.convert(menus, false);
        return buildTreeMenu(menuEntities);
    }

    @SneakyThrows
    @Override
    public void download(HttpServletResponse response) {
        ServletOutputStream outputStream = response.getOutputStream();
        response.setCharacterEncoding("UTF-8");

        List<MenuEntity> menus = menuRepository.findAll();
        byte[] bytes = objectMapper.writeValueAsBytes(menus);
        outputStream.write(bytes);
        outputStream.flush();
        outputStream.close();
    }

    private void findTargetChildrenMenuId(Long menuId, List<MenuEntity> menuEntities) {
        List<MenuEntity> menus = menuRepository.findByParentId(menuId);
        menus.forEach(menu -> {
            menu.setUpdated(LocalDateTime.now());
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