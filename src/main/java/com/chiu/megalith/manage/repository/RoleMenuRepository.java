package com.chiu.megalith.manage.repository;

import com.chiu.megalith.manage.entity.RoleMenuEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-11-27 11:53 am
 */
@Repository
public interface RoleMenuRepository extends JpaRepository<RoleMenuEntity, Long> {
    List<Long> findMenuIdsByRoleId(Long id);

    @Transactional
    void deleteByRoleId(Long roleId);
}
