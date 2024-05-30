package org.chiu.megalith.user.service;

import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.user.req.UserEntityRegisterReq;
import org.chiu.megalith.user.req.UserEntityReq;
import org.chiu.megalith.user.vo.UserEntityVo;

import java.util.List;

/**
 * @Author limingjiu
 * @Date 2024/5/29 22:12
 **/
public interface UserRoleService {

    void saveOrUpdate(UserEntityReq userEntityReq);

    void saveRegisterPage(String token, UserEntityRegisterReq userEntityRegisterReq);

    List<String> findRoleCodesByUserId(Long userId);

    List<String> findRoleCodesDecorByUserId(Long userId);


    UserEntityVo findById(Long userId);

    PageAdapter<UserEntityVo> listPage(Integer currentPage, Integer size);

    void deleteUsers(List<Long> ids);
}
