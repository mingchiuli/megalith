package org.chiu.megalith.manage.service.impl;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.service.RoleAuthorityService;
import org.chiu.megalith.manage.wrapper.RoleAuthorityWrapper;
import org.springframework.stereotype.Service;

import java.util.List;


@Service
@RequiredArgsConstructor
public class RoleAuthorityServiceImpl implements RoleAuthorityService {

   private final RoleAuthorityWrapper roleAuthorityWrapper;

    @Override
    public List<String> getAuthoritiesByRoleCode(String RoleCode) {
        return roleAuthorityWrapper.getAuthoritiesByRoleCode(RoleCode);
    }
}
