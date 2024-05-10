package org.chiu.megalith.authority.convertor;

import org.chiu.megalith.authority.entity.RoleAuthorityEntity;

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
