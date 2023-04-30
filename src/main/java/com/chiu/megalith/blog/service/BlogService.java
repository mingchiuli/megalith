package com.chiu.megalith.blog.service;

import com.chiu.megalith.blog.dto.BlogEntityDto;
import com.chiu.megalith.blog.entity.BlogEntity;
import com.chiu.megalith.blog.vo.*;
import com.chiu.megalith.infra.page.PageAdapter;
import org.springframework.data.domain.Pageable;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-11-27 2:12 pm
 */
public interface BlogService {

    List<Long> findIds(Pageable pageRequest);

    BlogExhibitVo findById(Long id, boolean visible);

    void setReadCount(Long id);

    BlogEntity findById(Long id);

    PageAdapter<BlogDescriptionVo> findPage(Integer currentPage, Integer year);

    Integer getCountByYear(Integer year);

    boolean checkToken(Long blogId, String token);

    Integer findStatusById(Long blogId);

    List<Integer> searchYears();

    Long count();

    void saveOrUpdate(BlogEntityVo blog);

    void deleteBlogs(List<Long> ids);

    String setBlogToken();

    PageAdapter<BlogEntityDto> findAllABlogs(Integer currentPage, Integer size);

    void recoverDeletedBlog(Long id, Integer idx);

    PageAdapter<BlogEntity> findDeletedBlogs(Integer currentPage, Integer size);

    void changeBlogStatus(Long id, Integer status);

    boolean exist(Long blogId);

    VisitStatisticsVo getVisitStatistics();

    List<BlogHotReadVo> getScoreBlogs();

    BlogEntity findByIdAndUserId(Long id, long userId);
}
