package org.chiu.megalith.manage.service.impl;

import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.manage.entity.RoleEntity;
import org.chiu.megalith.manage.entity.RoleMenuEntity;
import org.chiu.megalith.manage.repository.RoleRepository;
import org.chiu.megalith.manage.service.RoleService;
import org.chiu.megalith.manage.req.RoleEntityReq;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.vo.RoleEntityVo;
import org.chiu.megalith.manage.wrapper.RoleWrapper;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static org.chiu.megalith.infra.lang.ExceptionMessage.ROLE_NOT_EXIST;

/**
 * @author mingchiuli
 * @create 2022-12-04 2:26 am
 */
@Service
@RequiredArgsConstructor
public class RoleServiceImpl implements RoleService {

    private final RoleRepository roleRepository;

    private final RoleWrapper roleWrapper;

    @Override
    public RoleEntityVo info(Long id) {
        RoleEntity roleEntity = roleRepository.findById(id)
                .orElseThrow(() -> new MissException(ROLE_NOT_EXIST));

        return RoleEntityVo.builder()
                .code(roleEntity.getCode())
                .name(roleEntity.getName())
                .remark(roleEntity.getRemark())
                .status(roleEntity.getStatus())
                .id(roleEntity.getId())
                .build();
    }

    @Override
    public PageAdapter<RoleEntityVo> getPage(Integer currentPage, Integer size) {
        var pageRequest = PageRequest.of(currentPage - 1,
                size,
                Sort.by("created").ascending());
        Page<RoleEntity> page = roleRepository.findAll(pageRequest);

        List<RoleEntityVo> content = new ArrayList<>();
        page.getContent().forEach(role -> {
            RoleEntityVo roleEntityVo = RoleEntityVo.builder()
                    .code(role.getCode())
                    .name(role.getName())
                    .remark(role.getRemark())
                    .status(role.getStatus())
                    .updated(role.getUpdated())
                    .created(role.getCreated())
                    .id(role.getId())
                    .build();
            content.add(roleEntityVo);
        });

        return PageAdapter.<RoleEntityVo>builder()
                .empty(page.isEmpty())
                .first(page.isFirst())
                .last(page.isLast())
                .pageNumber(page.getPageable().getPageNumber())
                .content(content)
                .totalPages(page.getTotalPages())
                .pageSize(page.getSize())
                .totalElements(page.getTotalElements())
                .build();
    }

    @Override
    public void saveOrUpdate(RoleEntityReq roleVo) {

        Long id = roleVo.getId();
        RoleEntity roleEntity;
        var now = LocalDateTime.now();

        if (Objects.nonNull(id)) {
            roleEntity = roleRepository.findById(id)
                    .orElseThrow(() -> new MissException(ROLE_NOT_EXIST));
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
    public void delete(List<Long> ids) {
        roleWrapper.delete(ids);
    }


    @Override
    public List<Long> savePerm(Long roleId, List<Long> menuIds) {
        List<RoleMenuEntity> roleMenuEntities = menuIds.stream()
                .map(menuId -> RoleMenuEntity.builder()
                        .menuId(menuId)
                        .roleId(roleId)
                        .build())
                .toList();
        roleWrapper.savePerm(roleId, roleMenuEntities);
        return menuIds;
    }

    @Override
    public List<RoleEntityVo> getValidAll() {
        List<RoleEntity> entities = roleRepository.findByStatus(StatusEnum.NORMAL.getCode());
        List<RoleEntityVo> vos = new ArrayList<>();
        entities.forEach(item -> vos.add(RoleEntityVo.builder()
                .code(item.getCode())
                .id(item.getId())
                .status(item.getStatus())
                .name(item.getName())
                .build()));
        return vos;
    }
}
