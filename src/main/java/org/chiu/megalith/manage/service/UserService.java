package org.chiu.megalith.manage.service;


import org.chiu.megalith.manage.req.UserEntityReq;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.manage.vo.UserEntityVo;

import java.time.LocalDateTime;
import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 4:55 pm
 */
public interface UserService {

    void updateLoginTime(String username, LocalDateTime time);

    void saveOrUpdate(UserEntityReq userEntityReq);

    UserEntityVo findById(Long userId);

    void changeUserStatusByUsername(String username, Integer status);

    PageAdapter<UserEntityVo> listPage(Integer currentPage, Integer size);

    void deleteUsers(List<Long> ids);

    List<Long> findIdsByStatus(Integer status);

    UserEntityVo findByEmail(String email);

    UserEntityVo findByPhone(String loginSMS);
}
