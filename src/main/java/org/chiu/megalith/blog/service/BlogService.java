package org.chiu.megalith.blog.service;

import org.chiu.megalith.blog.dto.BlogEntityDto;
import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.vo.*;
import org.chiu.megalith.infra.page.PageAdapter;
import org.springframework.data.domain.Pageable;

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

    BlogEntity saveOrUpdate(BlogEntityVo blog, Long userId);

    PageAdapter<BlogEntityDto> findAllABlogs(Integer currentPage, Integer size, Long userId, String authority);

    BlogEntity recoverDeletedBlog(Long id, Integer idx, Long userId);

    PageAdapter<BlogEntity> findDeletedBlogs(Integer currentPage, Integer size, Long userId);

    Integer changeBlogStatus(Long id, Integer status);

    Boolean exist(Long blogId);

    VisitStatisticsVo getVisitStatistics();

    List<BlogHotReadVo> getScoreBlogs();

    BlogEntity findByIdAndUserId(Long id, Long userId);

    Long findUserIdById(Long id);

    void delete(BlogEntity blogEntity);

    Integer checkStatusByIdAndUserId(Long blogId, Long userId);
}
