package org.chiu.megalith.authority.convertor;

import org.chiu.megalith.authority.entity.RoleMenuEntity;

import java.util.List;

public class RoleMenuEntityConvertor {

    public static List<RoleMenuEntity> convert(Long roleId, List<Long> menuIds) {
        return  menuIds.stream()
                .map(menuId -> RoleMenuEntity.builder()
                        .menuId(menuId)
                        .roleId(roleId)
                        .build())
                .toList();
    }
}
