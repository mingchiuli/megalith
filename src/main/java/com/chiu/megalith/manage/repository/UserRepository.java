package com.chiu.megalith.manage.repository;

import com.chiu.megalith.manage.entity.UserEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-11-27 11:53 am
 */
@Repository
public interface UserRepository extends JpaRepository<UserEntity, Long> {

    Optional<UserEntity> findByUsernameOrEmailOrPhone(String username, String email, String phone);


    @Query(value = "SELECT user.id from UserEntity user where user.status = :status")
    List<Long> findByStatus(Integer status);


    @Query(value = "UPDATE UserEntity user set user.lastLogin = ?2 where user.username = ?1")
    @Modifying
    @Transactional
    void updateLoginTime(String username, LocalDateTime time);

    @Query(value = "SELECT new UserEntity (id, username, avatar) from UserEntity where (username = ?1 or email = ?1 or phone = ?1)")
    Optional<UserEntity> retrieveUserInfo(String username);

    @Query(value = "UPDATE UserEntity user set user.status = :status where user.username = :username")
    @Modifying
    @Transactional
    void setUserStatusByUsername(String username, Integer status);

    @Query(value = "UPDATE UserEntity user set user.status = :status where user.id = :id")
    @Modifying
    @Transactional
    void setUserStatusById(Long id, Integer status);

    @Query(value = "SELECT user.nickname from UserEntity user where user.id = ?1")
    Optional<String> findNicknameById(Long id);
}
