package org.chiu.megalith.manage.service;

import jakarta.servlet.http.HttpServletResponse;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.manage.req.BlogEntityReq;
import org.chiu.megalith.manage.vo.BlogDeleteVo;
import org.chiu.megalith.manage.vo.BlogEditVo;
import org.chiu.megalith.manage.vo.BlogEntityVo;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

public interface BlogManagerService {

    void saveOrUpdate(BlogEntityReq blog, Long userId);

    PageAdapter<BlogEntityVo> findAllABlogs(Integer currentPage, Integer size, Long userId, String role);

    void recoverDeletedBlog(Integer idx, Long userId);

    PageAdapter<BlogDeleteVo> findDeletedBlogs(Integer currentPage, Integer size, Long userId);

    BlogEditVo findEdit(Long id, Long userId);

    void deleteBatch(List<Long> ids, Long userId, String role);

    String uploadOss(MultipartFile image, Long userId);

    void deleteOss(String url);

    String setBlogToken(Long blogId);

    void download(HttpServletResponse response);
}
