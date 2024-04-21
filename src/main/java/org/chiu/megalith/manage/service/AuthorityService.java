package org.chiu.megalith.manage.service;

import jakarta.servlet.http.HttpServletResponse;
import org.chiu.megalith.manage.req.AuthorityEntityReq;
import org.chiu.megalith.manage.vo.AuthorityVo;

import java.util.List;

public interface AuthorityService {

    List<AuthorityVo> findAll();

    AuthorityVo findById(Long id);

    void saveOrUpdate(AuthorityEntityReq req);

    void deleteAuthorities(List<Long> ids);

    void download(HttpServletResponse response);
}
