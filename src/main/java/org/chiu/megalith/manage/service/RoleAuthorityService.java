package org.chiu.megalith.manage.service;


import org.chiu.megalith.manage.vo.RoleAuthorityVo;

import java.util.ArrayList;
import java.util.List;

public interface RoleAuthorityService {

    List<String> getAuthoritiesByRoleCode(String RoleCode);

    List<RoleAuthorityVo> getAuthoritiesInfo(Long roleId);

    void saveAuthority(Long roleId, ArrayList<Long> authorityIds);
}
