package com.chiu.megalith.backstage.service.impl;


import com.chiu.megalith.backstage.entity.RoleEntity;
import com.chiu.megalith.backstage.entity.RoleMenuEntity;
import com.chiu.megalith.backstage.repository.RoleRepository;
import com.chiu.megalith.backstage.service.RoleMenuService;
import com.chiu.megalith.backstage.service.RoleService;
import com.chiu.megalith.backstage.vo.RoleEntityVo;
import com.chiu.megalith.common.exception.NotFoundException;
import com.chiu.megalith.common.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

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
        Optional<RoleEntity> roleEntity = roleRepository.findById(id);
        return roleEntity.orElseThrow(() -> new NotFoundException("role not exist"));
    }

    @Override
    public PageAdapter<RoleEntity> listPage(Integer currentPage, Integer size) {
        Pageable pageRequest = PageRequest.of(currentPage - 1,
                size,
                Sort.by("created").ascending());
        Page<RoleEntity> page = roleRepository.findAll(pageRequest);
        return new PageAdapter<>(page);
    }

    @Override
    public void saveOrUpdate(RoleEntityVo roleVo) {
        var ref = new Object() {
            RoleEntity roleEntity;
        };

        LocalDateTime now = LocalDateTime.now();

        Optional.ofNullable(roleVo.getId()).ifPresentOrElse((id) -> {
            ref.roleEntity = roleRepository.findById(id).orElseThrow(() -> new NotFoundException("role not exist"));
            ref.roleEntity.setUpdated(now);
        }, () -> ref.roleEntity = RoleEntity.
                builder().
                created(now).
                updated(now).
                build()
        );

        BeanUtils.copyProperties(roleVo, ref.roleEntity);
        roleRepository.save(ref.roleEntity);
    }

    @Override
    public List<Long> getNavMenuIds(String role) {
        RoleEntity roleEntity = roleRepository.findByCode(role).orElseThrow(() -> new NotFoundException("role not exist"));
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
    public List<Long> perm(Long roleId, List<Long> menuIds) {

        List<RoleMenuEntity> roleMenus = menuIds.stream().map(menuId ->
                RoleMenuEntity.builder().
                        menuId(menuId).
                        roleId(roleId).
                        build()).
                toList();

        roleMenuService.deleteByRoleId(roleId);
        roleMenuService.saveAll(roleMenus);
        return menuIds;
    }
}
