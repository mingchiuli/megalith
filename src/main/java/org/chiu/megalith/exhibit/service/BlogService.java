package org.chiu.megalith.exhibit.service;

import org.chiu.megalith.exhibit.vo.BlogDescriptionVo;
import org.chiu.megalith.exhibit.vo.BlogExhibitVo;
import org.chiu.megalith.exhibit.vo.BlogHotReadVo;
import org.chiu.megalith.exhibit.vo.VisitStatisticsVo;
import org.chiu.megalith.infra.page.PageAdapter;
import org.springframework.data.domain.Pageable;
import org.springframework.security.core.Authentication;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-11-27 2:12 pm
 */
public interface BlogService {

    PageAdapter<BlogDescriptionVo> findPage(Integer currentPage, Integer year);

    BlogExhibitVo getLockedBlog(Long blogId, String token);

    Boolean checkToken(Long blogId, String token);

    Integer getBlogStatus(Authentication authentication, Long blogId);

    List<Integer> searchYears();

    VisitStatisticsVo getVisitStatistics();

    List<BlogHotReadVo> getScoreBlogs();

    BlogExhibitVo getBlogDetail(Authentication authentication, Long id);

    Long getCountByYear(Integer year);

    Long count();

    List<Integer> getYears();

    List<Long> findIds(Pageable pageRequest);

}
