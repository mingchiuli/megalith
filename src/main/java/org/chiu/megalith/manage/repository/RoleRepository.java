package org.chiu.megalith.manage.repository;

import org.chiu.megalith.manage.entity.RoleEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-11-27 11:52 am
 */
public interface RoleRepository extends JpaRepository<RoleEntity, Long> {

    Optional<RoleEntity> findByCode(String role);

    Optional<RoleEntity> findByCodeAndStatus(String role, Integer status);

    List<RoleEntity> findByStatus(Integer status);
}
