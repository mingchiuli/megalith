package org.chiu.megalith.user.service;

import jakarta.servlet.http.HttpServletResponse;
import org.chiu.megalith.user.req.AuthorityEntityReq;
import org.chiu.megalith.user.vo.AuthorityVo;

import java.util.List;

public interface AuthorityService {

    List<AuthorityVo> findAll();

    AuthorityVo findById(Long id);

    void saveOrUpdate(AuthorityEntityReq req);

    void deleteAuthorities(List<Long> ids);

    void download(HttpServletResponse response);
}
