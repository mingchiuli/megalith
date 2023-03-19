package com.chiu.megalith.exhibit.service;

import com.chiu.megalith.exhibit.dto.BlogEntityDto;
import com.chiu.megalith.exhibit.entity.BlogEntity;
import com.chiu.megalith.exhibit.vo.BlogExhibitVo;
import com.chiu.megalith.manage.vo.BlogEntityVo;
import com.chiu.megalith.base.page.PageAdapter;
import org.springframework.data.domain.Pageable;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-11-27 2:12 pm
 */
public interface BlogService {

    List<Long> findIdsByStatus(Integer status, Pageable pageRequest);
    BlogExhibitVo findByIdAndStatus(Long id, Integer status);

    void setReadCount(Long id);

    BlogEntity findById(Long id);


    PageAdapter<BlogEntity> findPage(Integer currentPage);

    PageAdapter<BlogEntity> findPageByYear(Integer currentPage, Integer year);

    Integer getCountByYear(Integer year);

    BlogEntity getLockedBlog(Long blogId, String token);

    Integer findStatusById(Long blogId);

    List<Integer> searchYears();

    Long count();

    void saveOrUpdate(BlogEntityVo blog);

    void deleteBlogs(List<Long> ids);

    void setBlogToken();

    String getBlogToken();

    PageAdapter<BlogEntityDto> findAllABlogs(Integer currentPage, Integer size);

    void recoverDeletedBlog(Long id);

    PageAdapter<BlogEntity> findDeletedBlogs(Integer currentPage, Integer size);

    void changeBlogStatus(Long id, Integer status, Integer year);

    boolean exist(Long blogId);
}
