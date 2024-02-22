package org.chiu.megalith.manage.convertor;

import org.chiu.megalith.manage.entity.RoleAuthorityEntity;

import java.util.List;

public class RoleAuthorityEntityConvertor {
    public static List<RoleAuthorityEntity> convert(Long roleId, List<Long> authorityIds) {
        return authorityIds.stream()
                .map(authorityId -> RoleAuthorityEntity.builder()
                        .authorityId(authorityId)
                        .roleId(roleId)
                        .build())
                .toList();
    }
}
