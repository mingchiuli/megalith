package org.chiu.megalith.authority.service;

import jakarta.servlet.http.HttpServletResponse;
import org.chiu.megalith.authority.req.AuthorityEntityReq;
import org.chiu.megalith.authority.vo.AuthorityVo;

import java.util.List;

public interface AuthorityService {

    List<AuthorityVo> findAll();

    AuthorityVo findById(Long id);

    void saveOrUpdate(AuthorityEntityReq req);

    void deleteAuthorities(List<Long> ids);

    void download(HttpServletResponse response);
}
