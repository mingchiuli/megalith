package org.chiu.megalith.blog.service;

import org.chiu.megalith.blog.req.BlogEditPushAllReq;
import org.chiu.megalith.blog.req.BlogEntityReq;
import org.chiu.megalith.blog.vo.*;
import org.chiu.megalith.infra.page.PageAdapter;
import org.springframework.data.domain.Pageable;
import org.springframework.security.core.Authentication;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-11-27 2:12 pm
 */
public interface BlogService {

    List<Long> findIds(Pageable pageRequest);

    PageAdapter<BlogDescriptionVo> findPage(Integer currentPage, Integer year);

    Integer getCountByYear(Integer year);

    List<Integer> searchYears();

    Long count();

    void saveOrUpdate(BlogEntityReq blog, Long userId);

    PageAdapter<BlogEntityVo> findAllABlogs(Integer currentPage, Integer size, Long userId, String role);

    void recoverDeletedBlog(Integer idx, Long userId);

    PageAdapter<BlogDeleteVo> findDeletedBlogs(Integer currentPage, Integer size, Long userId);

    VisitStatisticsVo getVisitStatistics();

    List<BlogHotReadVo> getScoreBlogs();

    BlogEditVo findEdit(Long id, Long userId);


    void deleteBatch(List<Long> ids, Long userId, String role);

    String uploadOss(MultipartFile image, Long userId);

    void deleteOss(String url);

    String setBlogToken(Long blogId);

    List<Integer> getYears();

    BlogExhibitVo getLockedBlog(Long blogId, String token);

    Boolean checkToken(Long blogId, String token);

    BlogExhibitVo getBlogDetail(Authentication authentication, Long id);

    Integer getBlogStatus(Authentication authentication, Long blogId);

    void pushAll(BlogEditPushAllReq blog, Long userId);
}
