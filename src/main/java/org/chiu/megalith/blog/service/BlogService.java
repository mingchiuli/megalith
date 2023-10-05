package org.chiu.megalith.blog.service;

import org.chiu.megalith.blog.vo.BlogEntityVo;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.vo.*;
import org.chiu.megalith.infra.page.PageAdapter;
import org.chiu.megalith.manage.req.BlogEntityReq;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-11-27 2:12 pm
 */
public interface BlogService {

    List<Long> findIds(Pageable pageRequest);

    BlogExhibitVo findById(Long id, Boolean visible);

    void setReadCount(Long id);

    BlogEntity findById(Long id);

    PageAdapter<BlogDescriptionVo> findPage(Integer currentPage, Integer year);

    Integer getCountByYear(Integer year);

    Boolean checkToken(Long blogId, String token);

    Integer findStatusById(Long blogId);

    List<Integer> searchYears();

    Long count();

    void saveOrUpdate(BlogEntityReq blog, Long userId);

    PageAdapter<BlogEntityVo> findAllABlogs(Integer currentPage, Integer size, Long userId, String authority);

    void recoverDeletedBlog(Long id, Integer idx, Long userId);

    PageAdapter<BlogDeleteVo> findDeletedBlogs(Integer currentPage, Integer size, Long userId);

    Boolean exist(Long blogId);

    VisitStatisticsVo getVisitStatistics();

    List<BlogHotReadVo> getScoreBlogs();

    BlogEditVo findEdit(Long id, Long userId);

    Long findUserIdById(Long id);

    void deleteBatch(List<Long> ids, Long userId, String authority);

    Integer checkStatusByIdAndUserId(Long blogId, Long userId);

    String uploadOss(MultipartFile image, Long userId);

    void deleteOss(String url);

    void setBlogStatus(Long id, Long userId, Integer status, String authority);

    String setBlogToken(Long blogId);

}
