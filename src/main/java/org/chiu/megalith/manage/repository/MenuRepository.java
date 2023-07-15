package org.chiu.megalith.manage.repository;


import org.chiu.megalith.manage.entity.MenuEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

/**
 * @author mingchiuli
 * @create 2022-11-27 11:50 am
 */
public interface MenuRepository extends JpaRepository<MenuEntity, Long> {
    List<MenuEntity> findAllByOrderByOrderNumDesc();
}
