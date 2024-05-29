package org.chiu.megalith.user.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import lombok.SneakyThrows;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.user.convertor.MenuDisplayVoConvertor;
import org.chiu.megalith.user.convertor.MenuEntityConvertor;
import org.chiu.megalith.user.convertor.MenuEntityVoConvertor;
import org.chiu.megalith.user.entity.MenuEntity;
import org.chiu.megalith.user.repository.MenuRepository;
import org.chiu.megalith.user.service.MenuService;
import org.chiu.megalith.user.req.MenuEntityReq;
import org.chiu.megalith.infra.exception.MissException;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.user.vo.MenuDisplayVo;
import org.chiu.megalith.user.vo.MenuEntityVo;
import org.chiu.megalith.user.wrapper.MenuWrapper;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.*;

import static org.chiu.megalith.infra.lang.ExceptionMessage.*;
import static org.chiu.megalith.user.convertor.MenuDisplayVoConvertor.buildTreeMenu;

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
        response.setCharacterEncoding(StandardCharsets.UTF_8.name());

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
}
