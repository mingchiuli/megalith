package com.chiu.megalith.backstage.repository;


import com.chiu.megalith.backstage.entity.MenuEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-11-27 11:50 am
 */
@Repository
public interface MenuRepository extends JpaRepository<MenuEntity, Long> {
    List<MenuEntity> findAllByOrderByOrderNumDesc();
}
