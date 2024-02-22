package org.chiu.megalith.manage.service;


import java.util.List;

public interface RoleAuthorityService {

    List<String> getAuthoritiesByRoleCode(String RoleCode);
}
