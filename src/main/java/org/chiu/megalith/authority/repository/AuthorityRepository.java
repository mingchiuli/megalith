package org.chiu.megalith.authority.repository;

import org.chiu.megalith.authority.entity.AuthorityEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface AuthorityRepository extends JpaRepository<AuthorityEntity, Long> {

    List<AuthorityEntity> findByStatus(Integer status);

}
