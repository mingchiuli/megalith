package org.chiu.megalith.user.convertor;

import org.chiu.megalith.user.entity.RoleMenuEntity;

import java.util.List;

public class RoleMenuEntityConvertor {

    private RoleMenuEntityConvertor() {}

    public static List<RoleMenuEntity> convert(Long roleId, List<Long> menuIds) {
        return  menuIds.stream()
                .map(menuId -> RoleMenuEntity.builder()
                        .menuId(menuId)
                        .roleId(roleId)
                        .build())
                .toList();
    }
}
