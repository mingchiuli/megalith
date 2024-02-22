package org.chiu.megalith.manage.repository;

import org.chiu.megalith.manage.entity.RoleAuthorityEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface RoleAuthorityRepository extends JpaRepository<RoleAuthorityEntity, Long> {

    void deleteByRoleId(Long roleId);

    List<RoleAuthorityEntity> findByRoleId(Long id);
}
