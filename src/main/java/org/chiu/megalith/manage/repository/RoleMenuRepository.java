package org.chiu.megalith.manage.repository;

import org.chiu.megalith.manage.entity.RoleMenuEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-11-27 11:53 am
 */
public interface RoleMenuRepository extends JpaRepository<RoleMenuEntity, Long> {
    
    @Query(value = "SELECT roleMenu.menuId from RoleMenuEntity roleMenu where roleMenu.roleId = ?1")
    List<Long> findMenuIdsByRoleId(Long id);

    void deleteByRoleId(Long roleId);

    void deleteByMenuId(Long menuId);
}
