package com.chiu.megalith.manage.service.impl;


import com.chiu.megalith.manage.entity.RoleEntity;
import com.chiu.megalith.manage.entity.RoleMenuEntity;
import com.chiu.megalith.manage.repository.RoleRepository;
import com.chiu.megalith.manage.service.RoleMenuService;
import com.chiu.megalith.manage.service.RoleService;
import com.chiu.megalith.manage.vo.RoleEntityVo;
import com.chiu.megalith.infra.exception.NotFoundException;
import com.chiu.megalith.infra.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:26 am
 */
@Service
@RequiredArgsConstructor
public class RoleServiceImpl implements RoleService {

    private final RoleRepository roleRepository;

    private final RoleMenuService roleMenuService;

    @Override
    public RoleEntity info(Long id) {
        return roleRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("role not exist"));
    }

    @Override
    public PageAdapter<RoleEntity> listPage(Integer currentPage,
                                            Integer size) {
        var pageRequest = PageRequest.of(currentPage - 1,
                size,
                Sort.by("created").ascending());
        Page<RoleEntity> page = roleRepository.findAll(pageRequest);
        return new PageAdapter<>(page);
    }

    @Override
    public void saveOrUpdate(RoleEntityVo roleVo) {

        Long id = roleVo.getId();
        RoleEntity roleEntity;
        var now = LocalDateTime.now();

        if (Objects.nonNull(id)) {
            roleEntity = roleRepository.findById(id)
                    .orElseThrow(() -> new NotFoundException("role not exist"));
            roleEntity.setUpdated(now);
        } else {
            roleEntity = RoleEntity.builder()
                    .created(now)
                    .updated(now)
                    .build();
        }

        BeanUtils.copyProperties(roleVo, roleEntity);
        roleRepository.save(roleEntity);
    }

    @Override
    public List<Long> getNavMenuIds(String role) {
        RoleEntity roleEntity = roleRepository.findByCode(role)
                .orElseThrow(() -> new NotFoundException("role not exist"));
        Long id = roleEntity.getId();
        return roleMenuService.findMenuIdsByRoleId(id);
    }


    @Override
    @Transactional
    public void delete(List<Long> ids) {
        ids.forEach(id -> {
            roleRepository.deleteById(id);
            roleMenuService.deleteByRoleId(id);
        });
    }


    @Override
    @Transactional
    public List<Long> perm(Long roleId,
                           List<Long> menuIds) {
        roleMenuService.deleteByRoleId(roleId);
        List<RoleMenuEntity> roleMenuEntities = menuIds.stream()
                .map(menuId -> RoleMenuEntity.builder()
                        .menuId(menuId)
                        .roleId(roleId)
                        .build())
                .toList();
        roleMenuService.saveAll(roleMenuEntities);
        return menuIds;
    }
}
