package org.chiu.megalith.blog.repository;

import org.chiu.megalith.blog.entity.BlogEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * @author mingchiuli
 * @create 2022-11-27 1:30 am
 */
public interface BlogRepository extends JpaRepository<BlogEntity, Long> {

    Page<BlogEntity> findAllByUserId(Pageable pageRequest, Long userId);

    Integer countByCreatedBetween(LocalDateTime start, LocalDateTime end);

    Long countByCreatedAfter(LocalDateTime created);

    Optional<BlogEntity> findByIdAndUserId(Long id, Long userId);

    @Query(value = "SELECT blog.userId from BlogEntity blog where blog.id = ?1")
    Long findUserIdById(Long id);

    @Query(value = "SELECT blog.status from BlogEntity blog where blog.id = ?1")
    Integer findStatusById(Long blogId);

    @Query(value = "SELECT blog.id from BlogEntity blog")
    List<Long> findIds(Pageable pageRequest);

    @Query(value = "SELECT count(blog) from BlogEntity blog where blog.created between :start and :end and blog.created < :created")
    Long getPageCountYear(@Param("created") LocalDateTime created, @Param("start") LocalDateTime start, @Param("end") LocalDateTime end);

    @Query(value = "UPDATE BlogEntity blog SET blog.status = :status WHERE blog.id = :id")
    @Modifying
    @Transactional
    void setStatus(@Param("id") Long id, @Param("status") Integer status);

    @Query(value = "UPDATE BlogEntity blog SET blog.readCount = blog.readCount + 1 WHERE blog.id = ?1")
    @Modifying
    @Transactional
    void setReadCount(Long id);

    @Query(value = "SELECT new BlogEntity (blog.id, blog.title, blog.description, blog.created, blog.link) from BlogEntity blog")
    Page<BlogEntity> findPage(Pageable pageRequest);

    @Query(value = "SELECT new BlogEntity (blog.id, blog.title, blog.description, blog.created, blog.link) from BlogEntity blog where blog.created between :start and :end")
    Page<BlogEntity> findPageByCreatedBetween(Pageable pageRequest, @Param("start") LocalDateTime start, @Param("end") LocalDateTime end);

    @Query(value = "SELECT DISTINCT(Year(blog.created)) from BlogEntity blog")
    List<Integer> getYears();
}
