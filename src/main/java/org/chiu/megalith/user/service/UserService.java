package org.chiu.megalith.user.service;


import jakarta.servlet.http.HttpServletResponse;
import org.springframework.web.multipart.MultipartFile;

import java.time.LocalDateTime;
import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 4:55 pm
 */
public interface UserService {

    void updateLoginTime(String username, LocalDateTime time);

    void changeUserStatusByUsername(String username, Integer status);

    List<Long> findIdsByStatus(Integer status);

    String getRegisterPage(String username);

    String imageUpload(String token, MultipartFile image);

    void imageDelete(String token, String url);

    Boolean checkRegisterPage(String token);

    void download(HttpServletResponse response);
}
