package org.chiu.megalith.manage.convertor;

import org.chiu.megalith.manage.entity.RoleMenuEntity;

import java.util.List;

public class RoleMenuEntityConvertor {

    public static List<RoleMenuEntity> convertor(Long roleId, List<Long> menuIds) {
        return  menuIds.stream()
                .map(menuId -> RoleMenuEntity.builder()
                        .menuId(menuId)
                        .roleId(roleId)
                        .build())
                .toList();
    }
}
