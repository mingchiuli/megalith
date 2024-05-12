package org.chiu.megalith.authority.service;


import jakarta.servlet.http.HttpServletResponse;
import org.chiu.megalith.authority.req.RoleEntityReq;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.authority.vo.RoleEntityVo;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:25 am
 */
public interface RoleService {

    RoleEntityVo info(Long id);

    PageAdapter<RoleEntityVo> getPage(Integer current, Integer size);

    void saveOrUpdate(RoleEntityReq role);

    void delete(List<Long> ids);

    void download(HttpServletResponse response);

    List<RoleEntityVo> getValidAll();
}
