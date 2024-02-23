package org.chiu.megalith.manage.service.impl;

import org.chiu.megalith.infra.cache.CacheEvict;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.lang.StatusEnum;
import org.chiu.megalith.manage.convertor.RoleEntityVoConvertor;
import org.chiu.megalith.manage.convertor.RoleMenuEntityConvertor;
import org.chiu.megalith.manage.entity.RoleEntity;
import org.chiu.megalith.manage.entity.RoleMenuEntity;
import org.chiu.megalith.manage.repository.RoleRepository;
import org.chiu.megalith.manage.service.RoleService;
import org.chiu.megalith.manage.req.RoleEntityReq;
import org.chiu.megalith.infra.exception.MissException;
import org.chiu.megalith.infra.page.PageAdapter;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.vo.RoleEntityVo;
import org.chiu.megalith.manage.wrapper.RoleAuthorityWrapper;
import org.chiu.megalith.manage.wrapper.RoleWrapper;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
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

    private final RoleAuthorityWrapper roleAuthorityWrapper;

    @Override
    public RoleEntityVo info(Long id) {
        RoleEntity roleEntity = roleRepository.findById(id)
                .orElseThrow(() -> new MissException(ROLE_NOT_EXIST));

        return RoleEntityVoConvertor.convert(roleEntity);
    }

    @Override
    public PageAdapter<RoleEntityVo> getPage(Integer currentPage, Integer size) {
        var pageRequest = PageRequest.of(currentPage - 1,
                size,
                Sort.by("created").ascending());
        Page<RoleEntity> page = roleRepository.findAll(pageRequest);

        return RoleEntityVoConvertor.convert(page);
    }

    @Override
    @CacheEvict(prefix = {Const.HOT_AUTHORITIES})
    public void saveOrUpdate(RoleEntityReq roleReq) {

        Long id = roleReq.getId();
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

        BeanUtils.copyProperties(roleReq, roleEntity);
        roleRepository.save(roleEntity);
    }

    @Override
    public void delete(List<Long> ids) {
        roleWrapper.delete(ids);
    }


    @Override
    public void saveMenu(Long roleId, List<Long> menuIds) {
        List<RoleMenuEntity> roleMenuEntities = RoleMenuEntityConvertor.convert(roleId, menuIds);
        roleWrapper.saveMenu(roleId, roleMenuEntities);
    }

    @Override
    public List<RoleEntityVo> getValidAll() {
        List<RoleEntity> entities = roleRepository.findByStatus(StatusEnum.NORMAL.getCode());
        return RoleEntityVoConvertor.convert(entities);
    }
}
