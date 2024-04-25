package org.chiu.megalith.manage.repository;

import org.chiu.megalith.manage.entity.AuthorityEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface AuthorityRepository extends JpaRepository<AuthorityEntity, Long> {

    List<AuthorityEntity> findByStatus(Integer status);

}
