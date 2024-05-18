package org.chiu.megalith.authority.service;


import org.chiu.megalith.authority.vo.RoleAuthorityVo;

import java.util.List;

public interface RoleAuthorityService {

    List<String> getAuthoritiesByRoleCode(String roleCode);

    List<RoleAuthorityVo> getAuthoritiesInfo(Long roleId);

    void saveAuthority(Long roleId, List<Long> authorityIds);
}
