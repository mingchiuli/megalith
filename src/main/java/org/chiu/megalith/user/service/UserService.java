package org.chiu.megalith.user.service;


import jakarta.servlet.http.HttpServletResponse;
import org.chiu.megalith.user.req.UserEntityRegisterReq;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.user.vo.UserEntityVo;
import org.springframework.web.multipart.MultipartFile;

import java.time.LocalDateTime;
import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 4:55 pm
 */
public interface UserService {

    void updateLoginTime(String username, LocalDateTime time);

    UserEntityVo findById(Long userId);

    void changeUserStatusByUsername(String username, Integer status);

    PageAdapter<UserEntityVo> listPage(Integer currentPage, Integer size);

    void deleteUsers(List<Long> ids);

    List<Long> findIdsByStatus(Integer status);

    UserEntityVo findByEmail(String email);

    UserEntityVo findByPhone(String loginSMS);

    String getRegisterPage(String username);

    void saveRegisterPage(String token, UserEntityRegisterReq userEntityRegisterReq);

    String imageUpload(String token, MultipartFile image);

    void imageDelete(String token, String url);

    Boolean checkRegisterPage(String token);

    void download(HttpServletResponse response);
}
