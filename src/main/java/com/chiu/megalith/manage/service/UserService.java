package com.chiu.megalith.manage.service;


import com.chiu.megalith.manage.entity.UserEntity;
import com.chiu.megalith.manage.vo.UserEntityVo;
import com.chiu.megalith.infra.page.PageAdapter;

import java.time.LocalDateTime;
import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 4:55 pm
 */
public interface UserService {

    void updateLoginTime(String username, LocalDateTime time);

    void saveOrUpdate(UserEntityVo userEntityVo);

    UserEntity findById(Long userId);

    void changeUserStatusById(Long id, Integer status);

    void changeUserStatusByUsername(String username, Integer status);

    PageAdapter<UserEntity> listPage(Integer currentPage, Integer size);

    void deleteUsers(List<Long> ids);

    List<Long> findIdsByStatus(Integer status);
}
