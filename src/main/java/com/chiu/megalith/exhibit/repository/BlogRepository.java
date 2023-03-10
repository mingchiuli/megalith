package com.chiu.megalith.exhibit.repository;

import com.chiu.megalith.exhibit.entity.BlogEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-11-27 1:30 am
 */
@Repository
public interface BlogRepository extends JpaRepository<BlogEntity, Long> {

    Optional<BlogEntity> findByIdAndStatus(Long id, Integer status);

    Page<BlogEntity> findAllByUserId(Pageable pageRequest, Long userId);

    Page<BlogEntity> findAllByCreatedBetween(Pageable pageRequest, LocalDateTime start, LocalDateTime end);

    Integer countByCreatedBetween(LocalDateTime start, LocalDateTime end);

    Long countByCreatedAfter(LocalDateTime created);

    @Query(value = "SELECT blog.status from BlogEntity blog where blog.id = ?1")
    Integer findStatusById(Long blogId);

    @Query(value = "SELECT blog.id from BlogEntity blog where blog.status = :status")
    List<Long> findIdsByStatus(Integer status, Pageable pageRequest);

    @Query(value = "SELECT distinct year(blog.created) from BlogEntity blog order by year(blog.created)")
    List<Integer> searchYears();

    @Query(value = "SELECT count(blog) from BlogEntity blog where blog.created < :created and Year(blog.created) = :year")
    Long getPageCountYear(LocalDateTime created, int year);

    @Query(value = "UPDATE BlogEntity blog SET blog.status = :status WHERE blog.id = :id")
    @Modifying
    @Transactional
    void setStatus(Long id, Integer status);

    @Query(value = "UPDATE BlogEntity blog SET blog.readCount = blog.readCount + 1 WHERE blog.id = ?1")
    @Modifying
    @Transactional
    void setReadCount(Long id);
}
