package org.chiu.megalith.manage.service;


import org.chiu.megalith.manage.entity.RoleEntity;
import org.chiu.megalith.manage.vo.RoleEntityVo;
import org.chiu.megalith.infra.page.PageAdapter;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
public interface RoleService {

    RoleEntity info(Long id);

    PageAdapter<RoleEntity> getPage(Integer current, Integer size);

    void saveOrUpdate(RoleEntityVo role);

    void delete(List<Long> ids);

    List<Long> getNavMenuIds(String role);

    List<Long> perm(Long roleId, List<Long> menuIds);
}
