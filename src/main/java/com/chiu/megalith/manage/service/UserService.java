package com.chiu.megalith.manage.service;


import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.vo.UserEntityVo;
import com.chiu.megalith.base.page.PageAdapter;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-12-04 4:55 pm
 */
public interface UserService {


    UserEntity retrieveUserInfo(String username);

    void updateLoginTime(String username, LocalDateTime time);

    void saveOrUpdate(UserEntityVo userEntityVo);

    Optional<String> findUsernameById(Long id);

    UserEntity findById(Long userId);

    void changeUserStatusById(Long id, Integer status);

    void changeUserStatusByUsername(String username, Integer status);

    PageAdapter<UserEntity> listPage(Integer currentPage, Integer size);

    void deleteUsers(List<Long> ids);

    UserEntity findByIdWithoutPassword(Long id);

    List<Long> findIdsByStatus(Integer status);
}
