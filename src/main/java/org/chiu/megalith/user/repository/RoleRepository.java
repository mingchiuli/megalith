package org.chiu.megalith.user.repository;

import org.chiu.megalith.user.entity.RoleEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-11-27 11:52 am
 */
public interface RoleRepository extends JpaRepository<RoleEntity, Long> {

    List<RoleEntity> findByCodeIn(List<String> roles);

    List<RoleEntity> findByCodeInAndStatus(List<String> role, Integer status);

    List<RoleEntity> findByStatus(Integer status);

    @Query(value = "SELECT role.code from RoleEntity role")
    List<String> findAllCodes();
}
