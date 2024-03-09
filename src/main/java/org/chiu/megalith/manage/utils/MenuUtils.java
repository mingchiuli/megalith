package org.chiu.megalith.manage.utils;

import org.chiu.megalith.manage.vo.MenuDisplayVo;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;

public class MenuUtils {

    public static List<MenuDisplayVo> buildTreeMenu(List<MenuDisplayVo> menus) {
        //2.组装父子的树形结构
        //2.1 找到所有一级分类
        return menus.stream()
                .filter(menu -> menu.getParentId() == 0)
                .peek(menu-> menu.setChildren(getChildren(menu, menus)))
                .sorted(Comparator.comparingInt(menu -> Objects.isNull(menu.getOrderNum()) ? 0 : menu.getOrderNum()))
                .toList();
    }

    private static List<MenuDisplayVo> getChildren(MenuDisplayVo root, List<MenuDisplayVo> all) {
        return all.stream()
                .filter(menu -> Objects.equals(menu.getParentId(), root.getMenuId()))
                .peek(menu -> menu.setChildren(getChildren(menu, all)))
                .sorted(Comparator.comparingInt(menu -> Objects.isNull(menu.getOrderNum()) ? 0 : menu.getOrderNum()))
                .toList();
    }
}
