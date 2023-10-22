package org.chiu.megalith.manage.repository;

import org.chiu.megalith.manage.entity.UserEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-11-27 11:53 am
 */
public interface UserRepository extends JpaRepository<UserEntity, Long> {

    Optional<UserEntity> findByUsername(String username);

    Optional<UserEntity> findByEmail(String email);

    Optional<UserEntity> findByPhone(String phone);

    @Query(value = "SELECT user.id from UserEntity user where user.status = :status")
    List<Long> findByStatus(Integer status);


    @Query(value = "UPDATE UserEntity user set user.lastLogin = ?2 where user.username = ?1")
    @Modifying
    @Transactional
    void updateLoginTime(String username, LocalDateTime time);

    @Query(value = "UPDATE UserEntity user set user.status = :status where user.username = :username")
    @Modifying
    @Transactional
    void setUserStatusByUsername(String username, Integer status);
}
