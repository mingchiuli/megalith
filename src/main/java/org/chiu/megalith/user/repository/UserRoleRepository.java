package org.chiu.megalith.user.repository;

import org.chiu.megalith.user.entity.UserRoleEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserRoleRepository extends JpaRepository<UserRoleEntity, Long> {

}
