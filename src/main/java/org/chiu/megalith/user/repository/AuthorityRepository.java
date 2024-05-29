package org.chiu.megalith.user.repository;

import org.chiu.megalith.user.entity.AuthorityEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface AuthorityRepository extends JpaRepository<AuthorityEntity, Long> {

    List<AuthorityEntity> findByStatus(Integer status);

}
