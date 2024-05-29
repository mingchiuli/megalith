package org.chiu.megalith.user.repository;

import org.chiu.megalith.user.entity.UserRoleEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface UserRoleRepository extends JpaRepository<UserRoleEntity, Long> {

    List<UserRoleEntity> findByUserIdIn(List<Long> userIds);
}
