package org.chiu.megalith.user.service;


import org.chiu.megalith.user.vo.RoleAuthorityVo;

import java.util.List;

public interface RoleAuthorityService {

    List<String> getAuthoritiesByRoleCodes(List<String> roleCodes);

    List<RoleAuthorityVo> getAuthoritiesInfo(Long roleId);

    void saveAuthority(Long roleId, List<Long> authorityIds);
}
